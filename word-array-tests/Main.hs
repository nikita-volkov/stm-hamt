{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import STM.HAMT.Private.Prelude
import MTLPrelude
import qualified STM.HAMT.Private.WordArray as WordArray
import qualified Focus.Impure as Focus
import qualified Main.Update as Update


main = htfMain $ htf_thisModulesTests

prop_differentInterpretersProduceSameResults (update :: Update.Update Char ()) =
  Update.interpretMaybeList update ===
  fmap WordArray.toMaybeList (Update.interpretWordArray update)

prop_fromListIsIsomorphicToToList =
  forAll gen prop
  where
    gen = do
      indices <- (nub . sort) <$> listOf index
      mapM (liftA2 (flip (,)) char . pure) indices
      where
        index = choose (0, pred (WordArray.maxSize)) :: Gen Int
        char = arbitrary :: Gen Char
    prop list = 
      list === (WordArray.toList . WordArray.fromList) list

test_focusMLookup = do
  assertEqual (Just 'a') . fst =<< WordArray.focusM Focus.lookup 3 w
  assertEqual (Just 'b') . fst =<< WordArray.focusM Focus.lookup 7 w
  assertEqual Nothing . fst =<< WordArray.focusM Focus.lookup 1 w
  assertEqual Nothing . fst =<< WordArray.focusM Focus.lookup 11 w
  where
    w = WordArray.fromList [(3, 'a'), (7, 'b'), (14, 'c')]

test_foldable = do
  assertEqual "abdc" $ toList w
  where
    w = WordArray.fromList [(3, 'a'), (7, 'b'), (8, 'd'), (14, 'c')]
