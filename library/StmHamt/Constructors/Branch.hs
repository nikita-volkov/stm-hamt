module StmHamt.Constructors.Branch where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.IntOps as IntOps
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


singleton :: Int -> a -> Branch a
singleton hash a = LeavesBranch hash (pure a)

pair :: Int -> Int -> Branch a -> Int -> Branch a -> STM (Branch a)
pair depth hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = IntOps.indexAtDepth depth hash1
    index2 = IntOps.indexAtDepth depth hash2
    in if index1 == index2
      then do
        deeperBranch <- pair (IntOps.nextDepth depth) hash1 branch1 hash2 branch2
        var <- newTVar (SparseSmallArray.singleton index1 deeperBranch)
        return (BranchesBranch (Hamt var))
      else BranchesBranch . Hamt <$> newTVar (SparseSmallArray.pair index1 branch1 index2 branch2)
