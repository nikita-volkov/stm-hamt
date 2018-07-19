module StmHamt.UnfoldMs.SparseSmallArray where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.UnfoldM
import qualified StmHamt.UnfoldMs.SmallArray as SmallArrayUnfoldMs


elements :: Monad m => SparseSmallArray a -> UnfoldM m a
elements (SparseSmallArray _ array) = SmallArrayUnfoldMs.elements array
