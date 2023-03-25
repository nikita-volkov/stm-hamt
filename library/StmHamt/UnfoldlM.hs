module StmHamt.UnfoldlM where

import DeferredFolds.UnfoldlM
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.SmallArray as SmallArray
import StmHamt.Prelude hiding (all, filter)
import StmHamt.Types

hamtElements :: Hamt a -> UnfoldlM STM a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsUnfoldlM >>= branchElements

branchElements :: Branch a -> UnfoldlM STM a
branchElements = \case
  LeavesBranch _ array -> SmallArray.elementsUnfoldlM array
  BranchesBranch hamt -> hamtElements hamt
