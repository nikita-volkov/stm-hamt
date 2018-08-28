-- |
-- HAMT API,
-- optimized for a fast 'size' operation.
-- That however comes at the cost of a small overhead in the other operations.
module StmHamt.SizedHamt
(
  SizedHamt,
  new,
  newIO,
  null,
  size,
  focus,
  insert,
  lookup,
  reset,
  unfoldlM,
  listT,
)
where

import StmHamt.Prelude hiding (insert, lookup, delete, fold, null)
import StmHamt.Types
import qualified Focus as Focus
import qualified StmHamt.Hamt as Hamt


{-# INLINE new #-}
new :: STM (SizedHamt element)
new = SizedHamt <$> newTVar 0 <*> Hamt.new

{-# INLINE newIO #-}
newIO :: IO (SizedHamt element)
newIO = SizedHamt <$> newTVarIO 0 <*> Hamt.newIO

-- |
-- /O(1)/.
{-# INLINE null #-}
null :: SizedHamt element -> STM Bool
null (SizedHamt sizeVar _) = (== 0) <$> readTVar sizeVar

-- |
-- /O(1)/.
{-# INLINE size #-}
size :: SizedHamt element -> STM Int
size (SizedHamt sizeVar _) = readTVar sizeVar

{-# INLINE reset #-}
reset :: SizedHamt element -> STM ()
reset (SizedHamt sizeVar hamt) =
  do
    Hamt.reset hamt
    writeTVar sizeVar 0

{-# INLINE focus #-}
focus :: (Eq key, Hashable key) => Focus element STM result -> (element -> key) -> key -> SizedHamt element -> STM result
focus focus elementToKey key (SizedHamt sizeVar hamt) =
  do
    (result, sizeModifier) <- Hamt.focus newFocus elementToKey key hamt
    forM_ sizeModifier (modifyTVar' sizeVar)
    return result
  where
    newFocus = Focus.testingSizeChange (Just pred) Nothing (Just succ) focus

{-# INLINE insert #-}
insert :: (Eq key, Hashable key) => (element -> key) -> element -> SizedHamt element -> STM ()
insert elementToKey element (SizedHamt sizeVar hamt) =
  do
    inserted <- Hamt.insert elementToKey element hamt
    when inserted (modifyTVar' sizeVar succ)

{-# INLINE lookup #-}
lookup :: (Eq key, Hashable key) => (element -> key) -> key -> SizedHamt element -> STM (Maybe element)
lookup elementToKey key (SizedHamt _ hamt) = Hamt.lookup elementToKey key hamt

{-# INLINE unfoldlM #-}
unfoldlM :: SizedHamt a -> UnfoldlM STM a
unfoldlM (SizedHamt _ hamt) = Hamt.unfoldlM hamt

{-# INLINE listT #-}
listT :: SizedHamt a -> ListT STM a
listT (SizedHamt _ hamt) = Hamt.listT hamt
