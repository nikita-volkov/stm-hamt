module StmHamt.Accessors.Indices where

import StmHamt.Prelude hiding (toList, traverse_)
import StmHamt.Types
import qualified StmHamt.Prelude as Prelude


{-# NOINLINE maxSize #-}
maxSize :: Int
maxSize = finiteBitSize (undefined :: Indices)

{-# NOINLINE maxIndex #-}
maxIndex :: Int
maxIndex = pred maxSize

{-# NOINLINE allIndices #-}
allIndices :: [Index]
allIndices = [0 .. maxIndex]

-- |
-- A number of indexes, preceding this one.
{-# INLINE position #-}
position :: Index -> Indices -> Position
position i b = popCount (b .&. (bit i - 1))

{-# INLINE elem #-}
elem :: Index -> Indices -> Bool
elem = flip testBit

{-# INLINE size #-}
size :: Indices -> Int
size = popCount

{-# INLINE null #-}
null :: Indices -> Bool
null = (== 0)

{-# INLINE toList #-}
toList :: Indices -> [Index]
toList w = filter (testBit w) allIndices

{-# INLINE positions #-}
positions :: Indices -> [Position]
positions = enumFromTo 0 . pred . size
