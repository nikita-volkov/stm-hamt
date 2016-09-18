module STM.HAMT.Private.Nodes where

import STM.HAMT.Private.Prelude hiding (insert, lookup, delete, foldM, null)
import qualified STM.HAMT.Private.Prelude as Prelude
import qualified STM.HAMT.Private.WordArray as WordArray
import qualified STM.HAMT.Private.SizedArray as SizedArray
import qualified STM.HAMT.Private.Level as Level
import qualified Focus.Impure
import qualified ListT


type Nodes e = TVar (WordArray.WordArray (Node e))

data Node e = 
  Nodes {-# UNPACK #-} !(Nodes e) |
  Leaf {-# UNPACK #-} !Hash !e |
  Leaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(SizedArray.SizedArray e)

type Hash = Int

{-# INLINE new #-}
new :: STM (Nodes e)
new = newTVar WordArray.empty

{-# INLINE newIO #-}
newIO :: IO (Nodes e)
newIO = newTVarIO WordArray.empty

insert :: (row -> Bool) -> row -> Hash -> Level.Level -> Nodes row -> STM ()
insert testRow e h l ns = do
  a <- readTVar ns
  let write n = writeTVar ns $ WordArray.set i n a
  case WordArray.lookup i a of
    Nothing -> write (Leaf h e)
    Just n -> case n of
      Nodes ns' -> insert testRow e h (Level.succ l) ns'
      Leaf h' e' ->
        if h' == h
          then if testRow e'
            then write (Leaf h e)
            else write (Leaves h (SizedArray.pair e e'))
          else do
            nodes <- pair h (Leaf h e) h' (Leaf h' e') (Level.succ l)
            write (Nodes nodes)
      Leaves h' la ->
        if h' == h
          then case SizedArray.find testRow la of
            Just (lai, _) ->
              write (Leaves h' (SizedArray.insert lai e la))
            Nothing ->
              write (Leaves h' (SizedArray.append e la))
          else
            write . Nodes =<< pair h (Leaf h e) h' (Leaves h' la) (Level.succ l)
  where
    i = Level.hashIndex l h

pair :: Hash -> Node e -> Hash -> Node e -> Level.Level -> STM (Nodes e)
pair h1 n1 h2 n2 l =
  if i1 == i2
    then newTVar . WordArray.singleton i1 . Nodes =<< pair h1 n1 h2 n2 (Level.succ l)
    else newTVar $ WordArray.pair i1 n1 i2 n2
  where
    hashIndex = Level.hashIndex l
    i1 = hashIndex h1
    i2 = hashIndex h2

focus :: Focus.Impure.Focus row STM result -> (row -> Bool) -> Hash -> Level.Level -> Nodes row -> STM result
focus s testRow h l ns = do
  a <- readTVar ns
  (r, a'm) <- WordArray.focusM s' ai a
  maybe (return ()) (writeTVar ns) a'm
  return r
  where
    ai = Level.hashIndex l h
    s' = \case
      Nothing -> traversePair (return . fmap (Leaf h)) =<< s Nothing
      Just n -> case n of
        Nodes ns' -> do
          r <- focus s testRow h (Level.succ l) ns'
          null ns' >>= \case
            True -> return (r, Focus.Impure.Remove)
            False -> return (r, Focus.Impure.Keep)
        Leaf h' e' ->
          case h' == h of
            True -> 
              case testRow e' of
                True  -> 
                  traversePair (return . fmap (Leaf h)) =<< s (Just e')
                False -> 
                  traversePair processDecision =<< s Nothing
                  where
                    processDecision = \case
                      Focus.Impure.Set e -> 
                        return (Focus.Impure.Set (Leaves h (SizedArray.pair e e')))
                      _ -> 
                        return Focus.Impure.Keep
            False -> 
              traversePair processDecision =<< s Nothing
              where
                processDecision = \case
                  Focus.Impure.Set e -> do
                    ns' <- pair h (Leaf h e) h' (Leaf h' e') (Level.succ l)
                    return (Focus.Impure.Set (Nodes ns'))
                  _ -> return Focus.Impure.Keep
        Leaves h' a' ->
          case h' == h of
            True ->
              case SizedArray.find testRow a' of
                Just (i', e') -> 
                  s (Just e') >>= traversePair processDecision
                  where
                    processDecision = \case
                      Focus.Impure.Keep -> 
                        return Focus.Impure.Keep
                      Focus.Impure.Remove -> 
                        case SizedArray.delete i' a' of
                          a'' -> case SizedArray.null a'' of
                            False -> return (Focus.Impure.Set (Leaves h' a''))
                            True -> return Focus.Impure.Remove
                      Focus.Impure.Set e ->
                        return (Focus.Impure.Set (Leaves h' (SizedArray.insert i' e a')))
                Nothing -> 
                  s Nothing >>= traversePair processDecision
                  where
                    processDecision = \case
                      Focus.Impure.Set e ->
                        return (Focus.Impure.Set (Leaves h' (SizedArray.append e a')))
                      _ ->
                        return Focus.Impure.Keep
            False ->
              s Nothing >>= traversePair processDecision
              where
                processDecision = \case
                  Focus.Impure.Set e -> do
                    ns' <- pair h (Leaf h e) h' (Leaves h' a') (Level.succ l)
                    return (Focus.Impure.Set (Nodes ns'))
                  _ ->
                    return Focus.Impure.Keep

null :: Nodes e -> STM Bool
null = fmap WordArray.null . readTVar

foldM :: (a -> e -> STM a) -> a -> Level.Level -> Nodes e -> STM a
foldM step acc level = 
  readTVar >=> foldlM step' acc
  where
    step' acc' = \case
      Nodes ns -> foldM step acc' (Level.succ level) ns
      Leaf _ e -> step acc' e
      Leaves _ a -> SizedArray.foldM step acc' a

stream :: Level.Level -> Nodes e -> ListT.ListT STM e
stream l =
  lift . readTVar >=> ListT.fromFoldable >=> \case
    Nodes n -> stream (Level.succ l) n
    Leaf _ e -> return e
    Leaves _ a -> ListT.fromFoldable a

size :: Nodes e -> STM Int
size nodes =
  readTVar nodes >>= foldlM step 0
  where
    step a =
      fmap (a+) . nodeSize
      where
        nodeSize :: Node e -> STM Int
        nodeSize =
          \case
            Nodes nodes -> size nodes
            Leaf _ _ -> pure 1
            Leaves _ x -> pure (SizedArray.size x)

deleteAll :: Nodes e -> STM ()
deleteAll tvar = 
  writeTVar tvar WordArray.empty
