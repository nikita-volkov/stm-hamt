module StmHamt.ListT where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import ListT
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


hamtElements :: Hamt a -> ListT STM a
hamtElements (Hamt var) = tVarValue var >>= SparseSmallArray.elementsListT >>= branchElements

branchElements :: Branch a -> ListT STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElements hamt

tVarValue :: TVar a -> ListT STM a
tVarValue var = lift (readTVar var)
