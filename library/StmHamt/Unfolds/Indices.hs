module StmHamt.Unfolds.Indices where

import StmHamt.Prelude hiding (filter, all)
import StmHamt.Types
import DeferredFolds.Unfold
import qualified StmHamt.Accessors.Indices as IndicesAccessors


elements :: Indices -> Unfold Index
elements indices = filter (testBit indices) all

all :: Unfold Index
all = intsInRange 0 IndicesAccessors.maxIndex

positions :: Indices -> Unfold Position
positions indices = intsInRange 0 (pred (IndicesAccessors.size indices))
