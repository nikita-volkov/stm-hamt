module StmHamt.ListT where

import ListT
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.SmallArray as SmallArray
import StmHamt.Prelude hiding (all, filter)
import StmHamt.Types

hamtElements :: Hamt a -> ListT STM a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsListT >>= branchElements

branchElements :: Branch a -> ListT STM a
branchElements = \case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElements hamt

tVarValue :: TVar a -> ListT STM a
tVarValue var = lift (readTVar var)
