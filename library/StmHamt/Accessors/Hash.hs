module StmHamt.Accessors.Hash where

import StmHamt.Prelude hiding (mask)
import StmHamt.Types


{-# INLINE index #-}
index :: Hash -> Int
index hash = mask .&. hash

{-# INLINE step #-}
step :: Int
step = 5

{-# NOINLINE mask #-}
mask :: Int
mask = bit step - 1
