module StmHamt.Constructors.Indices where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Prelude as Prelude


{-# INLINE singleton #-}
singleton :: Index -> Indices
singleton = bit

{-# INLINE insert #-}
insert :: Index -> Indices -> Indices
insert i = (bit i .|.)

{-# INLINE invert #-}
invert :: Index -> Indices -> Indices
invert i = (bit i `xor`)

{-# INLINE fromList #-}
fromList :: [Index] -> Indices
fromList = Prelude.foldr (.|.) 0 . map bit

{-# INLINE pair #-}
pair :: Index -> Index -> Indices
pair i1 i2 = bit i1 .|. bit i2
