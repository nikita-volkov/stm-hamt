module STM.HAMT.Private.Hash where

import STM.HAMT.Private.Prelude hiding (mask)


type Hash =
  Int

{-# INLINE toIndex #-}
toIndex :: Hash -> Int
toIndex hash =
  mask .&. hash

{-# INLINE succLevel #-}
succLevel :: Hash -> Hash
succLevel hash =
  unsafeShiftR hash step

{-# INLINE step #-}
step :: Int
step =
  5

{-# NOINLINE mask #-}
mask :: Int
mask =
  bit step - 1
