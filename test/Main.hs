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

      listToListThruHamtInIo :: (Eq key, Hashable key, Eq value, Show (key, value)) => [(key, value)] -> IO [(key, value)]
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
          testCase "insert text with dups" $ let
            list :: [(Text, Int)]
            list = [("\44825\v8<\178sV\37709\59477\EM\n\SYN\34862\45730\57533\1643\1958i8\65022F]B\54233\9429A\RS\1797\54979\580@\2006\3400\ETX\ACK\63557\50825C\3741 \238\54817P\3258\54517\1081F5\16070\NAKR\1539\1193S\33229\1726&\778\20733\250\857\712C)\SYN1\17881\DC2\702\1446\61050S\5170s\ENQ\826\1379[7\53746\46137\55010\&0\1518:\2827R\54715\n\DEL\33260\1966\664\58929\64301\839\2980",0),("",0),("",0)]
            hashMapList = sort (HashMap.toList (HashMap.fromList list))
            hamtList = sort $ unsafePerformIO $ do
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> Hamt.insert fst pair hamt
              hamtToListInIo hamt
            in assertEqual (show hamtList) hashMapList hamtList
          ,
          testCase "insert text with dups using focus" $ let
            list :: [(Text, Int)]
            list = [("\44825\v8<\178sV\37709\59477\EM\n\SYN\34862\45730\57533\1643\1958i8\65022F]B\54233\9429A\RS\1797\54979\580@\2006\3400\ETX\ACK\63557\50825C\3741 \238\54817P\3258\54517\1081F5\16070\NAKR\1539\1193S\33229\1726&\778\20733\250\857\712C)\SYN1\17881\DC2\702\1446\61050S\5170s\ENQ\826\1379[7\53746\46137\55010\&0\1518:\2827R\54715\n\DEL\33260\1966\664\58929\64301\839\2980",0),("",0),("",0)]
            hashMapList = sort (HashMap.toList (HashMap.fromList list))
            hamtList = sort $ unsafePerformIO $ do
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> do
                traceM ("Inserting " <> show pair)
                Hamt.focus (Focus.insert pair) fst (fst pair) hamt
              hamtToListInIo hamt
            in assertEqual (show hamtList) hashMapList hamtList
          ,
          testCase "insert text with dups using focus 2" $ let
            list :: [(Text, Int)]
            list = [("",0),("\877925R\vGw{f}\1112191+",0),("",0)]
            hashMapList = sort (HashMap.toList (HashMap.fromList list))
            hamtList = sort $ unsafePerformIO $ do
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> do
                traceM ("Inserting " <> show pair)
                Hamt.focus (Focus.insert pair) fst (fst pair) hamt
              hamtToListInIo hamt
            in assertEqual (show hamtList) hashMapList hamtList
          ,
          testProperty "hashmap insertion isomorphism" $ \ (list :: [(Text, Int)]) -> let
            expectedList = sort (HashMap.toList (HashMap.fromList list))
            hamtList = sort $ unsafePerformIO $ do
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> Hamt.insert fst pair hamt
              hamtToListInIo hamt
            in expectedList === hamtList
          ,
          testProperty "hashmap insertion isomorphism using focus" $ \ (list :: [(Text, Int)]) -> let
            expectedList = sort (HashMap.toList (HashMap.fromList list))
            hamtList = sort $ unsafePerformIO $ do
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> Hamt.focus (Focus.insert pair) fst (fst pair) hamt
              hamtToListInIo hamt
            in expectedList === hamtList
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
