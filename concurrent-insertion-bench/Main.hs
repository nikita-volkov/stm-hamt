module Main where

import Rebase.Prelude
import Criterion.Main
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified STM.HAMT.Simple as A
import qualified Control.Concurrent.Async as B
import qualified System.Random.MWC.Monad as C
import qualified Focus.Impure as D
import qualified Rebase.Data.Text as E
import qualified Rebase.Data.Vector as F


-- * Transactions 
-------------------------

data TransactionF row n where
  Insert :: row -> n -> TransactionF row n
  deriving (Functor)

type Transaction row = Free (TransactionF row)


-- * Interpreters
-------------------------

type Interpreter container = 
  forall row. (Hashable row, Eq row) => container row -> forall result. Transaction row result -> STM result

specializedInterpreter :: Interpreter A.HAMT
specializedInterpreter container =
  iterM $ \case
    Insert row n -> A.insert row container >> n

focusInterpreter :: Interpreter A.HAMT
focusInterpreter container =
  iterM $ \case
    Insert row n -> A.focus (D.insert row) row container >> n


-- * Session and runners
-------------------------

-- | A list of transactions per thread.
type Session row = [[Transaction row ()]]

type SessionRunner = 
  forall row. (Hashable row, Eq row) => Session row -> IO ()

sessionRunner :: Interpreter A.HAMT -> SessionRunner
sessionRunner interpreter threadTransactions = do
  m <- atomically $ A.new
  void $ flip B.mapConcurrently threadTransactions $ \actions -> do
    forM_ actions $ atomically . interpreter m


-- * Generators
-------------------------

type Generator a = C.Rand IO a

transactionGenerator :: Generator (Transaction Text ())
transactionGenerator = do
  text <- textGenerator
  return $ Free $ Insert text (Pure ())

textGenerator :: Generator Text
textGenerator = do
  l <- length
  s <- replicateM l char
  return $! E.pack s
  where
    length =
      C.uniformR (7, 20)
    char =
      chr <$> C.uniformR (ord 'a', ord 'z')


-- * Utils
-------------------------

slices :: Int -> [a] -> [[a]]
slices size l =
  case splitAt size l of
    ([], _) -> []
    (a, b) -> a : slices size b


-- * Main
-------------------------

main = do
  allTransactions <- C.runWithSeed seed $ replicateM actionsNum transactionGenerator
  defaultMain $! flip map threadsNums $! \threadsNum ->
    let
      sliceSize = actionsNum `div` threadsNum
      threadTransactions = slices sliceSize allTransactions
      in 
        bgroup
          (shows threadsNum . showString "/" . shows sliceSize $ "")
          [
            bench "Focus-based" $ nfIO $
              sessionRunner focusInterpreter threadTransactions,
            bench "Specialized" $ nfIO $
              sessionRunner specializedInterpreter threadTransactions
          ]
  where
    seed =
      C.toSeed (F.fromList [1..7])
    actionsNum =
      100000
    threadsNums =
      [1, 2, 4, 6, 8, 12, 16, 32, 40, 52, 64, 80, 128]
