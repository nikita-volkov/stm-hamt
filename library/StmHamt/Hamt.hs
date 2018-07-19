module StmHamt.Hamt where

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


empty :: STM (Hamt a)
empty = Hamt <$> newTVar SparseSmallArrayConstructors.empty

pair :: Hash -> Branch a -> Hash -> Branch a -> STM (Hamt a)
pair hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = HashAccessors.index hash1
    index2 = HashAccessors.index hash2
    in if index1 == index2
      then pair (HashConstructors.succLevel hash1) branch1 (HashConstructors.succLevel hash2) branch2
      else Hamt <$> newTVar (SparseSmallArrayConstructors.pair index1 branch1 index2 branch2)

focus :: Eq a => Focus a STM b -> Hash -> (a -> Bool) -> Hamt a -> STM b
focus focus hash test hamt =
  {-# SCC "focus" #-} 
  let
    Focus conceal reveal = Focuses.onHamtElement hash test focus
    in fmap fst (reveal hamt)

-- |
-- Returns a flag, specifying, whether the size has been affected.
insert :: Eq a => Hash -> (a -> Bool) -> a -> Hamt a -> STM Bool
insert hash test element (Hamt var) =
  {-# SCC "insert" #-} 
  let
    index = HashAccessors.index hash
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
          BranchesBranch hamt -> insert (HashConstructors.succLevel hash) test element hamt
