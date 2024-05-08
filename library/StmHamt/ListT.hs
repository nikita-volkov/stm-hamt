module StmHamt.ListT where

import qualified Control.Concurrent.STM as STM
import ListT
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.SmallArray as SmallArray
import StmHamt.Prelude hiding (all, filter)
import StmHamt.Types

hamtElements :: Hamt a -> ListT STM a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsListT >>= branchElements

hamtElementsNonAtomic :: Hamt a -> ListT IO a
hamtElementsNonAtomic (Hamt var) = tVarValueIO var >>= By6Bits.elementsListT >>= branchElementsNonAtomic

branchElements :: Branch a -> ListT STM a
branchElements = \case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElements hamt

branchElementsNonAtomic :: Branch a -> ListT IO a
branchElementsNonAtomic = \case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElementsNonAtomic hamt

tVarValue :: TVar a -> ListT STM a
tVarValue var = lift (readTVar var)

tVarValueIO :: TVar a -> ListT IO a
tVarValueIO var = lift (readTVarIO var)
