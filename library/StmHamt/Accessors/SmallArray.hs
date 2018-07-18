module StmHamt.Accessors.SmallArray where

import StmHamt.Prelude
import StmHamt.Types


{-# INLINE findWithIndex #-}
findWithIndex :: (a -> Bool) -> SmallArray a -> Maybe (Int, a)
findWithIndex test array =
  {-# SCC "findWithIndex" #-} 
  let
    size = sizeofSmallArray array
    iterate index = if index < size
      then let
        element = indexSmallArray array index
        in if test element
          then Just (index, element)
          else iterate (succ index)
      else Nothing
    in iterate 0
