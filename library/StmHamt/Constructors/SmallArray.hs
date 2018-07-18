module StmHamt.Constructors.SmallArray where

import StmHamt.Prelude
import StmHamt.Types


{-# INLINE empty #-}
empty :: SmallArray a
empty = runSmallArray (newSmallArray 0 undefined)

-- |
-- Remove an element.
{-# INLINE unset #-}
unset :: Int -> SmallArray a -> SmallArray a
unset index array =
  {-# SCC "unset" #-}
  let size = sizeofSmallArray array
      newSize = pred size
      amountOfFollowingElements = newSize - index
      in runSmallArray $ do
        newMa <- newSmallArray newSize undefined
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa index array (succ index) amountOfFollowingElements
        return newMa

{-# INLINE set #-}
set :: Int -> a -> SmallArray a -> SmallArray a
set index a array =
  {-# SCC "set" #-} 
  let
    size = sizeofSmallArray array
    in runSmallArray $ do
      newMa <- newSmallArray size undefined
      copySmallArray newMa 0 array 0 size
      writeSmallArray newMa index a
      return newMa

{-# INLINE insert #-}
insert :: Int -> a -> SmallArray a -> SmallArray a
insert index a array =
  {-# SCC "insert" #-} 
  let
    size = sizeofSmallArray array
    newSize = sizeofSmallArray array
    nextIndex = succ index
    amountOfFollowingElements = size - index
    in runSmallArray $ do
      newMa <- newSmallArray newSize a
      copySmallArray newMa 0 array 0 index
      copySmallArray newMa nextIndex array index amountOfFollowingElements
      return newMa

{-# INLINE cons #-}
cons :: a -> SmallArray a -> SmallArray a
cons a array =
  {-# SCC "cons" #-} 
  let
    size = sizeofSmallArray array
    newSize = succ size
    in runSmallArray $ do
      newMa <- newSmallArray newSize a
      copySmallArray newMa 1 array 0 size
      return newMa

{-# INLINE orderedPair #-}
orderedPair :: Int -> e -> Int -> e -> SmallArray e
orderedPair i1 e1 i2 e2 =
  {-# SCC "orderedPair" #-} 
  runSmallArray $ if 
    | i1 < i2 -> do
      a <- newSmallArray 2 e1
      writeSmallArray a 1 e2
      return a
    | i1 > i2 -> do
      a <- newSmallArray 2 e1
      writeSmallArray a 0 e2
      return a
    | otherwise -> do
      a <- newSmallArray 1 e2
      return a
