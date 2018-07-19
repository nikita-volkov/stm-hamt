module StmHamt.Hamt
(
  Hamt,
  new,
  newIO,
  null,
  focus,
  insert,
  reset,
  unfoldM,
)
where

import StmHamt.Prelude hiding (empty, insert, update, lookup, delete)
import StmHamt.Types
import qualified Focus as Focus
import qualified StmHamt.Focuses as Focuses
import qualified StmHamt.Constructors.SmallArray as SmallArrayConstructors
import qualified StmHamt.Constructors.SparseSmallArray as SparseSmallArrayConstructors
import qualified StmHamt.Constructors.Hash as HashConstructors
import qualified StmHamt.Accessors.Hash as HashAccessors
import qualified StmHamt.Accessors.SparseSmallArray as SparseSmallArrayAccessors
import qualified StmHamt.Accessors.SmallArray as SmallArrayAccessors
import qualified StmHamt.UnfoldMs as UnfoldMs


new :: STM (Hamt a)
new = Hamt <$> newTVar SparseSmallArrayConstructors.empty

newIO :: IO (Hamt a)
newIO = Hamt <$> newTVarIO SparseSmallArrayConstructors.empty

pair :: Hash -> Branch a -> Hash -> Branch a -> STM (Hamt a)
pair hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = HashAccessors.index hash1
    index2 = HashAccessors.index hash2
    in if index1 == index2
      then pair (HashConstructors.succLevel hash1) branch1 (HashConstructors.succLevel hash2) branch2
      else Hamt <$> newTVar (SparseSmallArrayConstructors.pair index1 branch1 index2 branch2)

focus :: (Eq element, Eq key, Hashable key) => Focus element STM result -> (element -> key) -> key -> Hamt element -> STM result
focus focus elementToKey key = focusExplicitly focus (hash key) ((==) key . elementToKey)

focusExplicitly :: Eq a => Focus a STM b -> Hash -> (a -> Bool) -> Hamt a -> STM b
focusExplicitly focus hash test hamt =
  {-# SCC "focus" #-} 
  let
    Focus conceal reveal = Focuses.onHamtElement hash test focus
    in fmap fst (reveal hamt)

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insert :: (Eq element, Eq key, Hashable key) => (element -> key) -> element -> Hamt element -> STM Bool
insert elementToKey element = let
  !key = elementToKey element
  in insertExplicitly (hash key) ((==) key . elementToKey) element

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insertExplicitly :: Eq a => Hash -> (a -> Bool) -> a -> Hamt a -> STM Bool
insertExplicitly hash test element (Hamt var) =
  {-# SCC "insertExplicitly" #-} 
  let
    !index = HashAccessors.index hash
    in do
      branchArray <- readTVar var
      case SparseSmallArrayAccessors.lookup index branchArray of
        Nothing -> do
          writeTVar var $! SparseSmallArrayConstructors.insert index (LeavesBranch hash (pure element)) branchArray
          return True
        Just branch -> case branch of
          LeavesBranch leavesHash leavesArray -> if leavesHash == hash
            then case SmallArrayAccessors.findWithIndex test leavesArray of
              Just (leavesIndex, leavesElement) -> if element == leavesElement
                then return False
                else do
                  writeTVar var $! SparseSmallArrayConstructors.replace index (LeavesBranch hash (SmallArrayConstructors.set leavesIndex element leavesArray)) branchArray
                  return False
              Nothing -> do
                writeTVar var $! SparseSmallArrayConstructors.replace index (LeavesBranch hash (SmallArrayConstructors.cons element leavesArray)) branchArray
                return True
            else let
              nextHash = HashConstructors.succLevel hash
              nextLeavesHash = HashConstructors.succLevel leavesHash
              in do
                hamt <- pair nextHash (LeavesBranch nextHash (pure element)) nextLeavesHash (LeavesBranch nextLeavesHash leavesArray)
                writeTVar var $! SparseSmallArrayConstructors.replace index (BranchesBranch hamt) branchArray
                return True
          BranchesBranch hamt -> insertExplicitly (HashConstructors.succLevel hash) test element hamt

reset :: Hamt a -> STM ()
reset (Hamt branchSsaVar) = writeTVar branchSsaVar SparseSmallArrayConstructors.empty

unfoldM :: Hamt a -> UnfoldM STM a
unfoldM = UnfoldMs.hamtElements
