-- |
-- Utility focuses.
module STM.HAMT.Private.Focuses where

import STM.HAMT.Private.Prelude hiding (Const)
import Focus.Impure


{-# INLINE projectingInstruction #-}
projectingInstruction :: Monad m => (Instruction a -> c) -> Focus a m b -> Focus a m (b, c)
projectingInstruction fn =
  (liftM . liftM) $
  \ (output, instruction) -> ((output, fn instruction), instruction)

-- |
-- Extends the output with a flag,
-- saying whether an instruction, which is not 'Keep', has been produced.
{-# INLINE testingIfModifies #-}
testingIfModifies :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfModifies =
  projectingInstruction $
  \case
    Keep -> False
    _ -> True

-- |
-- Extends the output with a flag,
-- saying whether the 'Remove' instruction has been produced.
{-# INLINE testingIfRemoves #-}
testingIfRemoves :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfRemoves =
  projectingInstruction $
  \case
    Remove -> True
    _ -> False

-- |
-- Extends the output with a flag,
-- saying whether an item will be inserted.
-- That is, it didn't exist before and a Set instruction is produced.
{-# INLINE testingIfInserts #-}
testingIfInserts :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfInserts focus lookupResult=
  do
    (output, instruction) <- focus lookupResult
    return ((output, isNothing lookupResult && instructionIsReplace instruction), instruction)
  where
    instructionIsReplace =
      \case
        Set _ -> True
        _ -> False

{-# INLINE testingSizeChange #-}
testingSizeChange :: Monad m => c -> c -> c -> Focus a m b -> Focus a m (b, c)
testingSizeChange dec none inc focus lookupResult =
  fmap newDecision (focus lookupResult)
  where
    newDecision (output, instruction) =
      ((output, change), instruction)
      where
        change =
          case lookupResult of
            Nothing ->
              case instruction of
                Set _ -> inc
                _ -> none
            Just _ ->
              case instruction of
                Remove -> dec
                _ -> none
