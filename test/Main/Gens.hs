module Main.Gens where

import Prelude hiding (choose)
import Test.QuickCheck.Gen
import StmHamt.Hamt (Hamt)
import Focus (Focus(..))
import Main.Transaction (Transaction)
import qualified StmHamt.Hamt as StmHamt
import qualified Data.HashMap.Strict as HashMap
import qualified Main.Transaction as Transaction


key :: Gen Text
key = do
  length <- frequency [(1, pure 0), (10, choose (1, 3))]
  chars <- vectorOf length (choose ('a', 'd'))
  return (fromString chars)

value :: Gen Int
value = choose (0, 9)

lookupTransaction :: Gen Transaction
lookupTransaction = Transaction.lookup <$> key

insertTransaction :: Gen Transaction
insertTransaction = Transaction.insert <$> key <*> value

insertWithHashTransaction :: Gen Transaction
insertWithHashTransaction = do
  keyValue <- key
  valueValue <- value
  let hashValue = hash keyValue .&. 0b111
  return (Transaction.insertWithHash hashValue keyValue valueValue)

insertUsingFocusTransaction :: Gen Transaction
insertUsingFocusTransaction = Transaction.insertUsingFocus <$> key <*> value

deleteUsingFocusTransaction :: Gen Transaction
deleteUsingFocusTransaction = Transaction.deleteUsingFocus <$> key

incrementUsingAdjustFocusTransaction :: Gen Transaction
incrementUsingAdjustFocusTransaction = Transaction.incrementUsingAdjustFocus <$> key

seriesOfModificationsTransaction :: Gen Transaction
seriesOfModificationsTransaction = undefined

transaction :: Gen Transaction
transaction =
  frequency
    [
      (9, lookupTransaction),
      (2, insertTransaction),
      (2, insertUsingFocusTransaction),
      (5, insertWithHashTransaction),
      (9, deleteUsingFocusTransaction),
      (9, incrementUsingAdjustFocusTransaction)
    ]

hamt :: Gen (STM (Hamt (Text, Int)))
hamt = do
  list <- keyValueList
  return $ do
    hamt <- StmHamt.new
    forM_ list $ \ pair -> StmHamt.insert fst pair hamt
    return hamt

keyValueList :: Gen [(Text, Int)]
keyValueList = do
  size <- choose (0, 9)
  replicateM size ((,) <$> key <*> value)
