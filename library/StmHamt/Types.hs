{-# OPTIONS_GHC -funbox-strict-fields #-}
module StmHamt.Types where

import StmHamt.Prelude

{-|
STM-specialized Hash Array Mapped Trie,
extended with its size-tracking functionality,
allowing for a fast 'size' operation.
-}
data SizedHamt element = SizedHamt !(TVar Int) !(Hamt element)

{-|
STM-specialized Hash Array Mapped Trie.
-}
newtype Hamt element = Hamt (TVar (SparseSmallArray (Branch element)))

data Branch element = BranchesBranch !(Hamt element) | LeavesBranch !Int !(SmallArray element)
