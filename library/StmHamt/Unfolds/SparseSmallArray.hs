module StmHamt.Unfolds.SparseSmallArray where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.Unfold


elements :: SparseSmallArray e -> Unfold e
elements (SparseSmallArray _ array) = Unfold (\ f z -> foldl' f z array)
