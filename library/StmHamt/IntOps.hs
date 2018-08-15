module StmHamt.IntOps where

import StmHamt.Prelude hiding (mask, index)
import StmHamt.Types


{-# INLINE atDepth #-}
atDepth :: Int -> Int -> Int
atDepth depth hash = unsafeShiftR hash depth

{-# INLINE indexAtDepth #-}
indexAtDepth :: Int -> Int -> Int
indexAtDepth depth hash = index (atDepth depth hash)

{-# INLINE index #-}
index :: Int -> Int
index hash = mask .&. hash

{-# INLINE depthStep #-}
depthStep :: Int
depthStep = 5

{-# NOINLINE mask #-}
mask :: Int
mask = bit depthStep - 1

{-# INLINE nextDepth #-}
nextDepth :: Int -> Int
nextDepth = (+ depthStep)
