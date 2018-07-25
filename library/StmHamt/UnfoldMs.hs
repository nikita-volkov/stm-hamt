module StmHamt.UnfoldMs where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.UnfoldM
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


hamtElements :: Hamt a -> UnfoldM STM a
hamtElements (Hamt var) = tVarValue var >>= SparseSmallArray.elementsUnfoldM >>= branchElements

branchElements :: Branch a -> UnfoldM STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsUnfoldM array
  BranchesBranch hamt -> hamtElements hamt
