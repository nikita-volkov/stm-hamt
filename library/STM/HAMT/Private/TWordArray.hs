module STM.HAMT.Private.TWordArray where

import STM.HAMT.Private.Prelude hiding (insert, lookup, delete, foldM, null)
import qualified STM.HAMT.Private.WordArray as A
import qualified STM.HAMT.Private.Focuses as F
import qualified Focus.Impure as B
import qualified ListT as E


newtype TWordArray a =
  TWordArray (TVar (A.WordArray a))

{-# INLINE new #-}
new :: STM (TWordArray a)
new =
  TWordArray <$> newTVar A.empty

{-# INLINE newIO #-}
newIO :: IO (TWordArray a)
newIO =
  TWordArray <$> newTVarIO A.empty

{-# INLINE singleton #-}
singleton :: Int -> a -> STM (TWordArray a)
singleton index value =
  fmap TWordArray $
  newTVar (A.singleton index value)

{-# INLINE pair #-}
pair :: Int -> a -> Int -> a -> STM (TWordArray a)
pair index1 value1 index2 value2 =
  fmap TWordArray $
  newTVar (A.pair index1 value1 index2 value2)

{-# INLINE insert #-}
insert :: TWordArray a -> Int -> a -> STM ()
insert (TWordArray var) index value =
  modifyTVar' var (A.set index value)

{-# INLINE lookup #-}
lookup :: TWordArray a -> Int -> STM (Maybe a)
lookup (TWordArray var) index =
  readTVar var >>= A.lookupM index

{-# INLINE null #-}
null :: TWordArray a -> STM Bool
null (TWordArray var) =
  fmap A.null (readTVar var)

{-# INLINE stream #-}
stream :: TWordArray a -> E.ListT STM a
stream (TWordArray var) =
  lift (readTVar var) >>= E.fromFoldable

{-# INLINE size #-}
size :: TWordArray a -> STM Int
size (TWordArray var) =
  fmap A.size (readTVar var)

{-# INLINE deleteAll #-}
deleteAll :: TWordArray a -> STM ()
deleteAll (TWordArray var) = 
  writeTVar var A.empty

{-# INLINE focus #-}
focus :: B.Focus a STM b -> Int -> TWordArray a -> STM b
focus focus index (TWordArray var) =
  do
    wordArray <- readTVar var
    ((output, modified), newWordArray) <- A.focusM (F.testingIfModifies focus) index wordArray
    when modified (writeTVar var newWordArray)
    return output

{-# INLINE fold #-}
fold :: (b -> a -> STM b) -> b -> TWordArray a -> STM b
fold step init (TWordArray var) =
  readTVar var >>= foldlM step init
