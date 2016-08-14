module STM.HAMT.Private.Level where

import STM.HAMT.Private.Prelude hiding (mask)


-- |
-- The depth level of a node.
-- Must be a multiple of the 'step' value.
type Level = Int

{-# INLINE hashIndex #-}
hashIndex :: Int -> Level -> Int
hashIndex i l = mask .&. unsafeShiftR i l

{-# NOINLINE mask #-}
mask :: Int
mask = bit step - 1

{-# INLINE step #-}
step :: Int
step = 5

{-# INLINE succ #-}
succ :: Level -> Level
succ = (+ step)
