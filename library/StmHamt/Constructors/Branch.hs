module StmHamt.Constructors.Branch where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Accessors.Hash as HashAccessors
import qualified StmHamt.Constructors.Hash as HashConstructors
import qualified StmHamt.Constructors.SparseSmallArray as SparseSmallArrayConstructors


singleton :: Hash -> a -> Branch a
singleton hash a = LeavesBranch hash (pure a)

pair :: Hash -> Branch a -> Hash -> Branch a -> STM (Branch a)
pair hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = HashAccessors.index hash1
    index2 = HashAccessors.index hash2
    in if index1 == index2
      then pair (HashConstructors.succLevel hash1) branch1 (HashConstructors.succLevel hash2) branch2
      else BranchesBranch . Hamt <$> newTVar (SparseSmallArrayConstructors.pair index1 branch1 index2 branch2)
