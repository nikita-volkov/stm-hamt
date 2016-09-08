-- |
-- HAMT API with no overhead in most operations,
-- but with a costly 'size' operation.
module STM.HAMT.Simple
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
import qualified Focus.Impure as B


-- |
-- STM-specialized Hash Array Mapped Trie.
newtype HAMT row =
  HAMT (A.Nodes row)

{-# INLINE new #-}
new :: STM (HAMT row)
new =
  HAMT <$> A.new

{-# INLINE newIO #-}
newIO :: IO (HAMT row)
newIO =
  HAMT <$> A.newIO

{-# INLINE null #-}
null :: HAMT row -> STM Bool
null (HAMT nodes) =
  A.null nodes

-- |
-- ~ /O(n \/ 32)/.
{-# INLINE size #-}
size :: HAMT row -> STM Int
size (HAMT nodes) =
  A.size nodes

{-# INLINE focus #-}
focus :: (Eq row, Hashable row) => B.Focus row STM result -> row -> HAMT row -> STM result
focus focus row (HAMT nodes) =
  A.focus focus (hash row) row 0 nodes

{-# INLINE insert #-}
insert :: (Eq row, Hashable row) => row -> HAMT row -> STM ()
insert row (HAMT nodes) =
  void (A.insert (hash row) row 0 nodes)

{-# INLINE deleteAll #-}
deleteAll :: HAMT row -> STM ()
deleteAll (HAMT nodes) =
  A.deleteAll nodes

{-# INLINE fold #-}
fold :: (result -> row -> STM result) -> result -> HAMT row -> STM result
fold progress result (HAMT nodes) =
  A.fold progress result 0 nodes

{-# INLINE stream #-}
stream :: HAMT row -> ListT STM row
stream (HAMT nodes) =
  A.stream 0 nodes
