-- |
-- HAMT API,
-- optimized for a fast 'size' operation.
-- That however comes at a cost of a small overhead in the other operations.
module STM.HAMT.Sized
(
  HAMT,
  new,
  newIO,
  null,
  size,
  focus,
  insert,
  deleteAll,
  fold,
  stream,
)
where

import STM.HAMT.Private.Prelude hiding (insert, lookup, delete, fold, null)
import qualified STM.HAMT.Private.Nodes as A
import qualified STM.HAMT.Private.Focuses as C
import qualified Focus.Impure as B


-- |
-- STM-specialized Hash Array Mapped Trie,
-- extended with its size-tracking functionality,
-- allowing for a fast 'size' operation.
data HAMT row =
  HAMT !(A.Nodes row) !(TVar Int)

{-# INLINE new #-}
new :: STM (HAMT row)
new =
  HAMT <$> A.new <*> newTVar 0

{-# INLINE newIO #-}
newIO :: IO (HAMT row)
newIO =
  HAMT <$> A.newIO <*> newTVarIO 0

{-# INLINE null #-}
null :: HAMT row -> STM Bool
null (HAMT _ size) =
  (== 0) <$> readTVar size

-- |
-- /O(1)/.
{-# INLINE size #-}
size :: HAMT row -> STM Int
size (HAMT nodes size) =
  readTVar size

{-# INLINE focus #-}
focus :: (Eq row, Hashable row) => B.Focus row STM result -> row -> HAMT row -> STM result
focus focus row (HAMT nodes size) =
  do
    (result, sizeModifier) <- A.focus (C.testingSizeChange (Just pred) Nothing (Just succ) focus) (hash row) row 0 nodes
    forM_ sizeModifier (modifyTVar' size)
    return result

{-# INLINE insert #-}
insert :: (Eq row, Hashable row) => row -> HAMT row -> STM ()
insert row (HAMT nodes size) =
  do
    inserted <- A.insert (hash row) row 0 nodes
    when inserted (modifyTVar' size succ)

{-# INLINE deleteAll #-}
deleteAll :: HAMT row -> STM ()
deleteAll (HAMT nodes size) =
  do
    A.deleteAll nodes
    writeTVar size 0

{-# INLINE fold #-}
fold :: (result -> row -> STM result) -> result -> HAMT row -> STM result
fold progress result (HAMT nodes _) =
  A.fold progress result 0 nodes

{-# INLINE stream #-}
stream :: HAMT row -> ListT STM row
stream (HAMT nodes _) =
  A.stream 0 nodes
