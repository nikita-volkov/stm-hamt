module Main.Transaction where

import Prelude
import StmHamt.Hamt (Hamt)
import Focus (Focus(..))
import qualified StmHamt.Hamt as StmHamt
import qualified Data.HashMap.Strict as HashMap
import qualified Focus
import qualified Data.Text as Text


data Transaction = forall result. (Show result, Eq result) => Transaction {
  name :: Text,
  applyToHashMap :: HashMap Text Int -> (result, HashMap Text Int),
  applyToStmHamt :: Hamt (Text, Int) -> STM result
}

instance Show Transaction where
  show = Text.unpack . name

lookup :: Text -> Transaction
lookup key = let
  name = fromString ("lookup " <> show key)
  applyToHashMap hashMap = (HashMap.lookup key hashMap, hashMap)
  applyToStmHamt hamt = (fmap . fmap) snd (StmHamt.lookup fst key hamt)
  in Transaction name applyToHashMap applyToStmHamt

insert :: Text -> Int -> Transaction
insert key value = let
  name = fromString ("insert " <> show key <> " " <> show value)
  applyToHashMap = ((),) . HashMap.insert key value
  applyToStmHamt hamt = StmHamt.insert fst (key, value) hamt $> ()
  in Transaction name applyToHashMap applyToStmHamt

{-| A transaction which can be used to manipulate hash collision -}
insertWithHash :: Int -> Text -> Int -> Transaction
insertWithHash hash key value = let
  name = fromString ("insert " <> show key <> " " <> show value)
  applyToHashMap = ((),) . HashMap.insert key value
  applyToStmHamt hamt = StmHamt.insertExplicitly hash ((==) key . fst) (key, value) hamt $> ()
  in Transaction name applyToHashMap applyToStmHamt

focus :: (forall m. Monad m => Focus Int m ()) -> Text -> Transaction
focus focus@(Focus conceal reveal) key = let
  name = "focus"
  applyToHashMap hashMap = let
    Identity (result, change) = case HashMap.lookup key hashMap of
      Just existingValue -> reveal existingValue
      Nothing -> conceal
    newHashMap = case change of
      Focus.Leave -> hashMap
      Focus.Set newValue -> HashMap.insert key newValue hashMap
      Focus.Remove -> HashMap.delete key hashMap
    in (result, newHashMap)
  applyToStmHamt = let
    stmHamtFocus = Focus.mappingInput (key,) snd focus
    in StmHamt.focus stmHamtFocus fst key
  in Transaction name applyToHashMap applyToStmHamt

insertUsingFocus :: Text -> Int -> Transaction
insertUsingFocus key value = let
  name = fromString ("insertUsingFocus " <> show key <> " " <> show value)
  in (focus (Focus.insert value) key) { name = name }

deleteUsingFocus :: Text -> Transaction
deleteUsingFocus key = let
  name = fromString ("deleteUsingFocus " <> show key)
  in (focus Focus.delete key) { name = name }

incrementUsingAdjustFocus :: Text -> Transaction
incrementUsingAdjustFocus key = let
  name = fromString ("incrementUsingAdjustFocus " <> show key)
  in (focus (Focus.adjust succ) key) { name = name }
