module STM.HAMT.Private.Nodes where

import STM.HAMT.Private.Prelude hiding (insert, lookup, delete, fold, null)
import qualified STM.HAMT.Private.TWordArray as A
import qualified STM.HAMT.Private.WordArray as F
import qualified STM.HAMT.Private.SizedArray1 as B
import qualified STM.HAMT.Private.Hash as C
import qualified Focus.Impure as D
import qualified ListT as E


newtype Nodes row =
  Nodes (A.TWordArray (Node row))

data Node row = 
  Node_Nodes {-# UNPACK #-} !(Nodes row) |
  Node_Rows {-# UNPACK #-} !Int {-# UNPACK #-} !(B.SizedArray1 row)

{-# INLINE new #-}
new :: STM (Nodes row)
new =
  Nodes <$> A.new

{-# INLINE newIO #-}
newIO :: IO (Nodes row)
newIO =
  Nodes <$> A.newIO

-- |
-- Returns a flag, specifying, whether the size has been affected.
insert :: (row -> Bool) -> row -> C.Hash -> Nodes row -> STM Bool
insert existingRowTest row hash (Nodes (A.TWordArray var)) =
  {-# SCC "insert" #-}
  do
    nodeArray <- readTVar var
    case F.lookup index nodeArray of
      Nothing ->
        do
          writeTVar var (F.set index (Node_Rows hash (B.singleton row)) nodeArray)
          return True
      Just (Node_Nodes nodes2) ->
        insert existingRowTest row (C.succLevel hash) nodes2
      Just (Node_Rows foundHash foundRowArray) ->
        if foundHash == hash
          then 
            case B.find existingRowTest foundRowArray of
              Just (foundIndex, _) ->
                do
                  writeTVar var (F.set index (Node_Rows hash (B.insert foundIndex row foundRowArray)) nodeArray)
                  return False
              Nothing ->
                do
                  writeTVar var (F.set index (Node_Rows hash (B.append row foundRowArray)) nodeArray)
                  return True
          else
            do
              nodes2 <- pair (C.succLevel hash) (Node_Rows (C.succLevel hash) (B.singleton row)) (C.succLevel foundHash) (Node_Rows (C.succLevel foundHash) foundRowArray)
              writeTVar var (F.set index (Node_Nodes nodes2) nodeArray)
              return True
  where
    index =
      C.toIndex hash

pair :: Int -> Node row -> Int -> Node row -> STM (Nodes row)
pair hash1 node1 hash2 node2 =
  {-# SCC "pair" #-} 
  fmap Nodes $
  if index1 == index2
    then A.singleton index1 . Node_Nodes =<< pair (C.succLevel hash1) node1 (C.succLevel hash2) node2
    else A.pair index1 node1 index2 node2
  where
    index1 =
      C.toIndex hash1
    index2 =
      C.toIndex hash2

{-# INLINE null #-}
null :: Nodes row -> STM Bool
null (Nodes nodeArray) =
  A.null nodeArray

focus :: D.Focus row STM result -> (row -> Bool) -> C.Hash -> Nodes row -> STM result
focus rowFocus rowTest lookupHash (Nodes nodeArray) =
  {-# SCC "focus" #-} 
  A.focus nodeFocus nodeIndex nodeArray
  where
    nodeIndex =
      C.toIndex lookupHash
    nodeFocus =
      \case
        Nothing ->
          rowFocus Nothing >>= traversePair (return . fmap (Node_Rows lookupHash . B.singleton))
        Just node ->
          case node of
            Node_Nodes nodes ->
              do
                result <- focus rowFocus rowTest (C.succLevel lookupHash) nodes
                instruction <- fmap (bool D.Keep D.Remove) (null nodes)
                return (result, instruction)
            Node_Rows foundHash foundRowArray ->
              case lookupHash == foundHash of
                True ->
                  case B.find rowTest foundRowArray of
                    Just (foundIndex, foundRow) ->
                      fmap (fmap instruction) (rowFocus (Just foundRow))
                      where
                        instruction =
                          \case
                            D.Set newRow ->
                              D.Set (Node_Rows lookupHash (B.insert foundIndex newRow foundRowArray))
                            D.Remove ->
                              let
                                newRowArray =
                                  B.delete foundIndex foundRowArray
                                in 
                                  if B.null newRowArray
                                    then D.Remove
                                    else D.Set (Node_Rows lookupHash newRowArray)
                            D.Keep ->
                              D.Keep
                    Nothing ->
                      fmap (fmap instruction) (rowFocus Nothing)
                      where
                        instruction =
                          \case
                            D.Set newRow ->
                              D.Set (Node_Rows lookupHash (B.append newRow foundRowArray))
                            _ ->
                              D.Keep
                False ->
                  rowFocus Nothing >>= traversePair instruction
                  where
                    instruction =
                      \case
                        D.Set newRow ->
                          fmap (D.Set . Node_Nodes) $
                          pair (C.succLevel lookupHash) (Node_Rows (C.succLevel lookupHash) (B.singleton newRow)) (C.succLevel foundHash) (Node_Rows (C.succLevel foundHash) foundRowArray)
                        _ ->
                          return D.Keep

{-# INLINABLE fold #-}
fold :: (result -> row -> STM result) -> result -> Nodes row -> STM result
fold rowProgress result (Nodes nodeArray) =
  A.fold nodeProgress result nodeArray
  where
    nodeProgress result =
      \case
        Node_Nodes nodes -> fold rowProgress result nodes
        Node_Rows _ rowArray -> B.fold rowProgress result rowArray

{-# INLINABLE stream #-}
stream :: Nodes row -> E.ListT STM row
stream (Nodes nodeArray) =
  A.stream nodeArray >>= \case
    Node_Nodes nodes ->
      stream nodes
    Node_Rows _ rowArray ->
      E.fromFoldable rowArray

{-# INLINABLE size #-}
size :: Nodes row -> STM Int
size (Nodes nodeArray) =
  A.fold progress 0 nodeArray
  where
    progress result =
      fmap (result +) . nodeSize
      where
        nodeSize =
          \case
            Node_Nodes nodes -> size nodes
            Node_Rows _ rowArray -> pure (B.size rowArray)

{-# INLINE deleteAll #-}
deleteAll :: Nodes row -> STM ()
deleteAll (Nodes nodeArray) =
  A.deleteAll nodeArray
