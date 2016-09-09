module STM.HAMT.Private.Hash where

import STM.HAMT.Private.Prelude


type Hash =
  Int

{-# INLINE toIndex #-}
toIndex :: Hash -> Int
toIndex hash =
  (bit step - 1) .&. hash

{-# INLINE succLevel #-}
succLevel :: Hash -> Hash
succLevel hash =
  unsafeShiftR hash step

{-# INLINE step #-}
step :: Int
step =
  5
