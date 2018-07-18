module StmHamt.Constructors.SparseSmallArray where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Accessors.Indices as IndicesAccessors
import qualified StmHamt.Constructors.Indices as IndicesConstructors
import qualified StmHamt.Constructors.SmallArray as SmallArrayConstructors


{-# INLINE empty #-}
empty :: SparseSmallArray e
empty = SparseSmallArray 0 SmallArrayConstructors.empty

-- |
-- An array with a single element at the specified index.
{-# INLINE singleton #-}
singleton :: Int -> e -> SparseSmallArray e
singleton i e = 
  let b = IndicesConstructors.insert i 0
      a = runST $ newSmallArray 1 e >>= unsafeFreezeSmallArray
      in SparseSmallArray b a

{-# INLINE pair #-}
pair :: Int -> e -> Int -> e -> SparseSmallArray e
pair i1 e1 i2 e2 =
  {-# SCC "pair" #-} 
  SparseSmallArray indices array
  where 
    indices = IndicesConstructors.pair i1 i2
    array = SmallArrayConstructors.orderedPair i1 e1 i2 e2

{-|
Insert an element value at the index.
It's your obligation to ensure that the index is empty before the operation.
-}
{-# INLINE insert #-}
insert :: Int -> e -> SparseSmallArray e -> SparseSmallArray e
insert i e (SparseSmallArray b a) =
  {-# SCC "insert" #-} 
  let
    sparseIndex = IndicesAccessors.position i b
    in SparseSmallArray (IndicesConstructors.insert i b) (SmallArrayConstructors.insert sparseIndex e a)
    
{-# INLINE replace #-}
replace :: Int -> e -> SparseSmallArray e -> SparseSmallArray e
replace i e (SparseSmallArray b a) =
  {-# SCC "replace" #-} 
  let
    sparseIndex = IndicesAccessors.position i b
    in SparseSmallArray b (SmallArrayConstructors.set sparseIndex e a)
