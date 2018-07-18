module StmHamt.Accessors.SparseSmallArray where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Accessors.Indices as IndicesAccessors
import qualified StmHamt.Accessors.SmallArray as SmallArrayAccessors


-- |
-- Lookup an item at the index.
{-# INLINE lookup #-}
lookup :: Int -> SparseSmallArray e -> Maybe e
lookup i (SparseSmallArray b a) =
  {-# SCC "lookup" #-} 
  if IndicesAccessors.elem i b
    then Just (indexSmallArray a (IndicesAccessors.position i b))
    else Nothing
