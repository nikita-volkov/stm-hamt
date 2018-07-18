{-# OPTIONS_GHC -funbox-strict-fields #-}
module StmHamt.Types where

import StmHamt.Prelude


newtype Hamt element = Hamt (TVar (SparseSmallArray (Branch element)))

data Branch element = BranchesBranch !(Hamt element) | LeavesBranch !Hash !(SmallArray element)

{-|
An immutable space-efficient sparse array, 
which can store not more than 32 elements.
-}
data SparseSmallArray e = SparseSmallArray !Indices !(SmallArray e)

{-|
A compact set of indices.
-}
type Indices = Int

type Index = Int

type Position = Int

type Hash = Int
