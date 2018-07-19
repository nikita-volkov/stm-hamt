module StmHamt.UnfoldMs.SmallArray where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.UnfoldM


elements :: Monad m => SmallArray e -> UnfoldM m e
elements array = UnfoldM $ \ step initialState -> let
  !size = sizeofSmallArray array
  iterate index !state = if index < size
    then do
      element <- indexSmallArrayM array index
      newState <- step state element
      iterate (succ index) newState
    else return state
  in iterate 0 initialState
