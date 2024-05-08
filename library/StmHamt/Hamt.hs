module StmHamt.Hamt
  ( Hamt,
    new,
    newIO,
    null,
    focus,
    focusExplicitly,
    insert,
    insertExplicitly,
    lookup,
    lookupExplicitly,
    reset,
    unfoldlM,
    listT,
    listTNonAtomic,
  )
where

import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified StmHamt.Focuses as Focus
import qualified StmHamt.IntOps as IntOps
import qualified StmHamt.ListT as ListT
import StmHamt.Prelude hiding (delete, empty, insert, lookup, null)
import StmHamt.Types
import qualified StmHamt.UnfoldlM as UnfoldlM

new :: STM (Hamt a)
new = Hamt <$> newTVar By6Bits.empty

newIO :: IO (Hamt a)
newIO = Hamt <$> newTVarIO By6Bits.empty

focus :: (Hashable key) => Focus element STM result -> (element -> key) -> key -> Hamt element -> STM result
focus focus elementToKey key = focusExplicitly focus (hash key) ((==) key . elementToKey)

focusExplicitly :: Focus a STM b -> Int -> (a -> Bool) -> Hamt a -> STM b
focusExplicitly focus hash test hamt =
  {-# SCC "focus" #-}
  let Focus _ reveal = Focus.onHamtElement 0 hash test focus
   in fmap fst (reveal hamt)

-- |
-- Returns a flag, specifying, whether the size has been affected.
insert :: (Hashable key) => (element -> key) -> element -> Hamt element -> STM Bool
insert elementToKey element =
  let !key = elementToKey element
   in insertExplicitly (hash key) ((==) key . elementToKey) element

-- |
-- Returns a flag, specifying, whether the size has been affected.
insertExplicitly :: Int -> (a -> Bool) -> a -> Hamt a -> STM Bool
insertExplicitly hash testKey element =
  {-# SCC "insertExplicitly" #-}
  let loop depth (Hamt var) =
        let !branchIndex = IntOps.indexAtDepth depth hash
         in do
              branchArray <- readTVar var
              case By6Bits.lookup branchIndex branchArray of
                Nothing -> do
                  writeTVar var $! By6Bits.insert branchIndex (LeavesBranch hash (pure element)) branchArray
                  return True
                Just branch -> case branch of
                  LeavesBranch leavesHash leavesArray ->
                    if leavesHash == hash
                      then case SmallArray.findWithIndex testKey leavesArray of
                        Just (leavesIndex, _) ->
                          let !newLeavesArray = SmallArray.set leavesIndex element leavesArray
                              !newBranch = LeavesBranch hash newLeavesArray
                              !newBranchArray = By6Bits.replace branchIndex newBranch branchArray
                           in do
                                writeTVar var newBranchArray
                                return False
                        Nothing ->
                          let newLeavesArray = SmallArray.cons element leavesArray
                           in do
                                writeTVar var $! By6Bits.replace branchIndex (LeavesBranch hash newLeavesArray) branchArray
                                return True
                      else do
                        hamt <- pair (IntOps.nextDepth depth) hash (LeavesBranch hash (pure element)) leavesHash (LeavesBranch leavesHash leavesArray)
                        writeTVar var $! By6Bits.replace branchIndex (BranchesBranch hamt) branchArray
                        return True
                  BranchesBranch hamt -> loop (IntOps.nextDepth depth) hamt
   in loop 0

pair :: Int -> Int -> Branch a -> Int -> Branch a -> STM (Hamt a)
pair depth hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let index1 = IntOps.indexAtDepth depth hash1
      index2 = IntOps.indexAtDepth depth hash2
   in if index1 == index2
        then do
          deeperHamt <- pair (IntOps.nextDepth depth) hash1 branch1 hash2 branch2
          var <- newTVar (By6Bits.singleton index1 (BranchesBranch deeperHamt))
          return (Hamt var)
        else Hamt <$> newTVar (By6Bits.pair index1 branch1 index2 branch2)

-- |
-- Returns a flag, specifying, whether the size has been affected.
lookup :: (Hashable key) => (element -> key) -> key -> Hamt element -> STM (Maybe element)
lookup elementToKey key = lookupExplicitly (hash key) ((==) key . elementToKey)

lookupExplicitly :: Int -> (a -> Bool) -> Hamt a -> STM (Maybe a)
lookupExplicitly hash test =
  {-# SCC "lookupExplicitly" #-}
  let loop depth (Hamt var) =
        let !index = IntOps.indexAtDepth depth hash
         in do
              branchArray <- readTVar var
              case By6Bits.lookup index branchArray of
                Just branch -> case branch of
                  LeavesBranch leavesHash leavesArray ->
                    if leavesHash == hash
                      then return (SmallArray.find test leavesArray)
                      else return Nothing
                  BranchesBranch hamt -> loop (IntOps.nextDepth depth) hamt
                Nothing -> return Nothing
   in loop 0

reset :: Hamt a -> STM ()
reset (Hamt branchSsaVar) = writeTVar branchSsaVar By6Bits.empty

unfoldlM :: Hamt a -> UnfoldlM STM a
unfoldlM = UnfoldlM.hamtElements

listT :: Hamt a -> ListT STM a
listT = ListT.hamtElements

listTNonAtomic :: Hamt a -> ListT IO a
listTNonAtomic = ListT.hamtElementsNonAtomic

null :: Hamt a -> STM Bool
null (Hamt branchSsaVar) = do
  branchSsa <- readTVar branchSsaVar
  return (By6Bits.null branchSsa)
