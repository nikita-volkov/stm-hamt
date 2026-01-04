-- |
-- HAMT API,
-- optimized for a fast 'size' operation.
-- That however comes at the cost of a small overhead in the other operations.
module StmHamt.SizedHamt
  ( SizedHamt,
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
    listTNonAtomic,
  )
where

import qualified Focus as Focus
import qualified StmHamt.Hamt as Hamt
import qualified StmHamt.ListT as ListT
import StmHamt.Prelude hiding (delete, fold, insert, lookup, null)
import StmHamt.Types

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
focus :: (Hashable key) => Focus element STM result -> (element -> key) -> key -> SizedHamt element -> STM result
focus focus elementToKey key (SizedHamt sizeVar hamt) =
  do
    (result, sizeModifier) <- Hamt.focus newFocus elementToKey key hamt
    forM_ sizeModifier (modifyTVar' sizeVar)
    return result
  where
    newFocus = Focus.testingSizeChange (Just pred) Nothing (Just succ) focus

{-# INLINE insert #-}
insert :: (Hashable key) => (element -> key) -> element -> SizedHamt element -> STM ()
insert elementToKey element (SizedHamt sizeVar hamt) =
  do
    inserted <- Hamt.insert elementToKey element hamt
    when inserted (modifyTVar' sizeVar succ)

{-# INLINE lookup #-}
lookup :: (Hashable key) => (element -> key) -> key -> SizedHamt element -> STM (Maybe element)
lookup elementToKey key (SizedHamt _ hamt) = Hamt.lookup elementToKey key hamt

{-# INLINE unfoldlM #-}
unfoldlM :: SizedHamt a -> UnfoldlM STM a
unfoldlM (SizedHamt _ hamt) = Hamt.unfoldlM hamt

{-# INLINE listT #-}
listT :: SizedHamt a -> ListT STM a
listT (SizedHamt _ hamt) = Hamt.listT hamt

{-# INLINE listTNonAtomic #-}
listTNonAtomic :: SizedHamt a -> ListT IO a
listTNonAtomic (SizedHamt _ hamt) = ListT.hamtElementsNonAtomic hamt
