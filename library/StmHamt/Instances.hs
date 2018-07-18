module StmHamt.Instances where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Unfolds.SparseSmallArray as SparseSmallArrayUnfolds
import qualified StmHamt.Unfolds.Indices as IndicesUnfolds


instance Foldable SparseSmallArray where
  {-# INLINE foldr #-}
  foldr step state = foldr step state . SparseSmallArrayUnfolds.elements
  {-# INLINE foldl' #-}
  foldl' step state = foldl' step state . SparseSmallArrayUnfolds.elements
  {-# INLINE foldMap #-}
  foldMap monoid = foldMap monoid . SparseSmallArrayUnfolds.elements

