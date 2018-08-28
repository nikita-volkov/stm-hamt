module StmHamt.UnfoldlM where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.UnfoldlM
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


hamtElements :: Hamt a -> UnfoldlM STM a
hamtElements (Hamt var) = tVarValue var >>= SparseSmallArray.elementsUnfoldlM >>= branchElements

branchElements :: Branch a -> UnfoldlM STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsUnfoldlM array
  BranchesBranch hamt -> hamtElements hamt
