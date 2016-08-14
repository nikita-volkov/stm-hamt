module STM.HAMT.Private.Prelude
( 
  module Exports,
  traversePair,
  modifyTVar',
)
where

-- base
-------------------------
import BasePrelude as Exports

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- transformers
-------------------------
import Control.Monad.Trans.Class as Exports

-- list-t
-------------------------
import ListT as Exports (ListT)

-- | A replacement for the missing 'Traverse' instance of pair in base < 4.7.
{-# INLINE traversePair #-}
traversePair :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
traversePair f (x, y) = (,) x <$> f y

-- | Strict version of 'modifyTVar'.
{-# INLINE modifyTVar' #-}
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x
