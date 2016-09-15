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
focus :: (Eq key, Hashable key) => B.Focus row STM result -> (row -> key) -> key -> HAMT row -> STM result
focus focus rowToKey key (HAMT nodes) =
  A.focus focus ((==) key . rowToKey) (hash key) nodes

{-# INLINE insert #-}
insert :: (Eq key, Hashable key) => (row -> key) -> row -> HAMT row -> STM ()
insert rowToKey row (HAMT nodes) =
  void (A.insert ((==) key . rowToKey) row (hash key) nodes)
  where
    key =
      rowToKey row

{-# INLINE deleteAll #-}
deleteAll :: HAMT row -> STM ()
deleteAll (HAMT nodes) =
  A.deleteAll nodes

{-# INLINE fold #-}
fold :: (result -> row -> STM result) -> result -> HAMT row -> STM result
fold progress result (HAMT nodes) =
  A.fold progress result nodes

{-# INLINE stream #-}
stream :: HAMT row -> ListT STM row
stream (HAMT nodes) =
  A.stream nodes
