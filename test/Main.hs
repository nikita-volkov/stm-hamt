module Main where

import Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck
import Test.QuickCheck.Property hiding (testCase)
import StmHamt.Hamt (Hamt)
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
      hamtFromListUsingInsertInIo :: (Eq key, Hashable key, Eq value) => [(key, value)] -> IO (Hamt (key, value))
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
          testProperty "insert" $ forAll Gens.insertTransaction $ \ (Transaction.Transaction name applyToHashMap applyToStmHamt) -> let
            list = [("a", 1), ("b", 2)]
            hashMapList = HashMap.toList (snd (applyToHashMap (HashMap.fromList list)))
            hamtList = unsafePerformIO $ do
              hamt <- hamtFromListUsingInsertInIo list
              atomically $ applyToStmHamt hamt
              hamtToListInIo hamt
            in sort hashMapList === sort hamtList
        ]
  ]
