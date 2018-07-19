module StmHamt.UnfoldMs where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.UnfoldM
import qualified StmHamt.UnfoldMs.SmallArray as SmallArrayUnfoldMs
import qualified StmHamt.UnfoldMs.SparseSmallArray as SparseSmallArrayUnfoldMs


hamtElements :: Hamt a -> UnfoldM STM a
hamtElements (Hamt var) = tVarValue var >>= SparseSmallArrayUnfoldMs.elements >>= branchElements

branchElements :: Branch a -> UnfoldM STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArrayUnfoldMs.elements array
  BranchesBranch hamt -> hamtElements hamt
