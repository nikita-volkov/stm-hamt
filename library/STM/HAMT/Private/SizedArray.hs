module STM.HAMT.Private.SizedArray where

import STM.HAMT.Private.Prelude hiding (lookup, toList, fold)
import Data.Primitive.Array
import qualified STM.HAMT.Private.Prelude as Prelude

-- |
-- An array, 
-- which sacrifices the performance for space-efficiency and thread-safety.
data SizedArray a =
  SizedArray {-# UNPACK #-} !Int {-# UNPACK #-} !(Array a)

instance Foldable SizedArray where
  {-# INLINE foldr #-}
  foldr step r (SizedArray size array) =
    foldr step r $ map (indexArray array) [0 .. pred size]

-- |
-- An index of an element.
type Index = Int

empty :: SizedArray a
empty =
  runST $ do
    a <- newArray 0 undefined
    SizedArray 0 <$> unsafeFreezeArray a

{-# INLINE pair #-}
pair :: a -> a -> SizedArray a
pair e e' =
  runST $ do
    a <- newArray 2 e
    writeArray a 1 e'
    SizedArray 2 <$> unsafeFreezeArray a

-- |
-- Get the amount of elements.
{-# INLINE size #-}
size :: SizedArray a -> Int
size (SizedArray b _) = b

{-# INLINE null #-}
null :: SizedArray a -> Bool
null = (== 0) . size

{-# INLINE find #-}
find :: (a -> Bool) -> SizedArray a -> Maybe (Index, a)
find p (SizedArray s a) = loop 0
  where
    loop i = if i < s
      then let e = indexArray a i in if p e
        then Just (i, e)
        else loop (succ i)
      else Nothing

{-# INLINE indexOf #-}
indexOf :: Eq a => a -> SizedArray a -> Maybe Index
indexOf value (SizedArray size array) =
  recur 0
  where
    recur index =
      if index < size
        then if indexArray array index == value
          then Just index
          else recur (succ index)
        else Nothing

-- |
-- Unsafe. Doesn't check the index overflow.
{-# INLINE insert #-}
insert :: Index -> a -> SizedArray a -> SizedArray a
insert i e (SizedArray s a) = 
  runST $ do
    m' <- newArray s undefined
    forM_ [0 .. pred s] $ \i' -> indexArrayM a i' >>= writeArray m' i'
    writeArray m' i e
    SizedArray s <$> unsafeFreezeArray m'

{-# INLINE delete #-}
delete :: Index -> SizedArray a -> SizedArray a
delete i (SizedArray s a) = 
  runST $ do
    m' <- newArray (pred s) undefined
    forM_ [0 .. pred i] $ \i' -> indexArrayM a i' >>= writeArray m' i'
    forM_ [succ i .. pred s] $ \i' -> indexArrayM a i' >>= writeArray m' (pred i')
    SizedArray (pred s) <$> unsafeFreezeArray m'

{-# INLINE append #-}
append :: a -> SizedArray a -> SizedArray a
append e (SizedArray s a) =
  runST $ do
    m' <- newArray (succ s) undefined
    forM_ [0 .. pred s] $ \i -> indexArrayM a i >>= writeArray m' i
    writeArray m' s e
    SizedArray (succ s) <$> unsafeFreezeArray m'

{-# INLINE fold #-}
fold :: (Monad m) => (a -> b -> m a) -> a -> SizedArray b -> m a
fold step acc (SizedArray size array) =
  Prelude.foldM step' acc [0 .. pred size]
  where
    step' acc' i = indexArrayM array i >>= step acc'
