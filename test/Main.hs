module Main where

import Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property hiding (testCase, (.&.))
import StmHamt.Hamt (Hamt)
import Main.Transaction (Transaction)
import qualified Main.Transaction as Transaction
import qualified Main.Gens as Gens
import qualified StmHamt.Hamt as Hamt
import qualified Data.HashMap.Strict as HashMap
import qualified Focus
import qualified DeferredFolds.UnfoldM as UnfoldM


main =
  defaultMain $
  testGroup "All" $
  [
    testGroup "Hamt" $ let

      hamtFromListUsingInsertWithHashInIo :: (Eq key, Eq value, Show (key, value)) => (key -> Int) -> [(key, value)] -> IO (Hamt (key, value))
      hamtFromListUsingInsertWithHashInIo hash list = do
        hamt <- Hamt.newIO
        atomically $ forM_ list $ \ (key, value) -> Hamt.insertExplicitly (hash key) ((==) key . fst) (key, value) hamt
        return hamt

      hamtFromListUsingInsertInIo :: (Eq key, Hashable key, Eq value, Show (key, value)) => [(key, value)] -> IO (Hamt (key, value))
      hamtFromListUsingInsertInIo list = do
        hamt <- Hamt.newIO
        atomically $ forM_ list $ \ pair -> Hamt.insert fst pair hamt
        return hamt

      hamtToListInIo :: Hamt a -> IO [a]
      hamtToListInIo hamt =
        fmap reverse $
        atomically $
        UnfoldM.foldlM' (\ state element -> return (element : state)) [] (Hamt.unfoldM hamt)

      listToListThruHamtInIo :: [(Char, Int)] -> IO [(Char, Int)]
      listToListThruHamtInIo = hamtFromListUsingInsertInIo >=> hamtToListInIo

      testTransactionProperty :: String -> (Text -> Int) -> Gen Transaction -> TestTree
      testTransactionProperty name hash transactionGen =
        let
          gen = (,) <$> transactionGen <*> Gens.keyValueList
          in
            testProperty ("Transaction: " <> name) $
            forAll gen $ \ (Transaction.Transaction name applyToHashMap applyToStmHamt, list) -> let
              (result1, hashMapList) = fmap (sort . HashMap.toList) (applyToHashMap (HashMap.fromList list))
              (result2, hamtList) = unsafePerformIO $ do
                hamt <- hamtFromListUsingInsertWithHashInIo hash list
                result2 <- atomically $ applyToStmHamt hamt
                list <- hamtToListInIo hamt
                return (result2, sort list)
              in
                counterexample
                  ("hashMapList: " <> show hashMapList <> "\nhamtList: " <> show hamtList <> "\nresult1: " <> show result1 <> "\nresult2: " <> show result2)
                  (hashMapList == hamtList && result1 == result2)

      in
        [
          testCase "insert" $ let
            list =
              [
                ('a', 1),
                ('b', 2),
                ('c', 3),
                ('d', 4)
              ]
            in do
              hamtList <- listToListThruHamtInIo list
              assertEqual (show hamtList) list hamtList
          ,
          testCase "insert with dup" $ let
            list =
              [
                ('a', 1),
                ('b', 1),
                ('b', 2),
                ('c', 3),
                ('d', 4)
              ]
            in do
              hamtList <- listToListThruHamtInIo list
              assertEqual (show hamtList) (delete ('b', 1) list) hamtList
          ,
          testTransactionProperty "insert" hash Gens.insertTransaction
          ,
          let
            newHash key = hash key .&. 0b111
            in testTransactionProperty "Hash collision" newHash (Gens.insertWithHashTransaction newHash)
          ,
          testTransactionProperty "insertUsingFocus" hash Gens.insertUsingFocusTransaction
          ,
          testTransactionProperty "deleteUsingFocus" hash Gens.deleteUsingFocusTransaction
          ,
          testTransactionProperty "incrementUsingAdjustFocus" hash Gens.incrementUsingAdjustFocusTransaction
          ,
          testTransactionProperty "lookup" hash Gens.lookupTransaction
        ]
  ]
