module StmHamt.Constructors.Hash where

import StmHamt.Prelude
import StmHamt.Types
import qualified StmHamt.Accessors.Hash as HashAccessors


{-# INLINE succLevel #-}
succLevel :: Hash -> Hash
succLevel hash = unsafeShiftR hash HashAccessors.step
