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
import qualified Main.Gens as Gen
import qualified StmHamt.Hamt as Hamt
import qualified Data.HashMap.Strict as HashMap
import qualified Focus
import qualified DeferredFolds.UnfoldM as UnfoldM


main =
  defaultMain $
  testGroup "All" $
  [
    testGroup "Hamt" $ let
      hamtToListInIo :: Hamt a -> IO [a]
      hamtToListInIo hamt =
        fmap reverse $
        atomically $
        UnfoldM.foldlM' (\ state element -> return (element : state)) [] (Hamt.unfoldM hamt)
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
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> Hamt.insert fst pair hamt
              hamtList <- hamtToListInIo hamt
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
              hamt <- Hamt.newIO
              atomically $ forM_ list $ \ pair -> Hamt.insert fst pair hamt
              hamtList <- hamtToListInIo hamt
              assertEqual (show hamtList) (delete ('b', 1) list) hamtList
        ]
  ]
