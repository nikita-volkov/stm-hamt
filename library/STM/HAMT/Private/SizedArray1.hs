module STM.HAMT.Private.SizedArray1 where

import STM.HAMT.Private.Prelude hiding (insert, lookup, delete, fold, null)
import qualified STM.HAMT.Private.SizedArray as B
import qualified ListT as E


-- |
-- Sized array, which has at least one argument.
data SizedArray1 a =
  One !a |
  Many {-# UNPACK #-} !(B.SizedArray a)
  deriving (Foldable)


-- * Construction
-------------------------

{-# INLINE singleton #-}
singleton :: a -> SizedArray1 a
singleton =
  One

{-# INLINE pair #-}
pair :: a -> a -> SizedArray1 a
pair a b =
  Many (B.pair a b)

-- |
-- Get the amount of elements.
{-# INLINE size #-}
size :: SizedArray1 a -> Int
size =
  \case
    One _ ->
      1
    Many sizedArray ->
      B.size sizedArray

{-# INLINE null #-}
null :: SizedArray1 a -> Bool
null = (== 0) . size

{-# INLINE find #-}
find :: (a -> Bool) -> SizedArray1 a -> Maybe (Int, a)
find predicate =
  \case
    One value ->
      if predicate value
        then Just (0, value)
        else Nothing
    Many sizedArray ->
      B.find predicate sizedArray

{-# INLINE indexOf #-}
indexOf :: Eq a => a -> SizedArray1 a -> Maybe Int
indexOf value =
  \case
    One foundValue ->
      if value == foundValue
        then Just 0
        else Nothing
    Many sizedArray ->
      B.indexOf value sizedArray

-- |
-- Unsafe. Doesn't check the index overflow.
{-# INLINE insert #-}
insert :: Int -> a -> SizedArray1 a -> SizedArray1 a
insert index newValue =
  \case
    One _ ->
      One newValue
    Many sizedArray ->
      Many (B.insert index newValue sizedArray)

{-# INLINE delete #-}
delete :: Int -> SizedArray1 a -> SizedArray1 a
delete index =
  \case
    Many sizedArray ->
      Many (B.delete index sizedArray)
    One _ ->
      Many (B.empty)

{-# INLINE append #-}
append :: a -> SizedArray1 a -> SizedArray1 a
append value =
  \case
    One existingValue ->
      pair existingValue value
    Many sizedArray ->
      Many (B.append value sizedArray)

{-# INLINE fold #-}
fold :: (Monad m) => (a -> b -> m a) -> a -> SizedArray1 b -> m a
fold progress enter =
  \case
    One value ->
      progress enter value
    Many sizedArray ->
      B.fold progress enter sizedArray
