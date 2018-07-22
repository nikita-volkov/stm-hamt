{-|
Utility focuses.
-}
module StmHamt.Focuses where

import StmHamt.Prelude
import StmHamt.Types
import Focus
import qualified StmHamt.Accessors.Hash as HashAccessors
import qualified StmHamt.Constructors.Branch as BranchConstructors
import qualified StmHamt.Constructors.Hash as HashConstructors
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray
import qualified PrimitiveExtras.SmallArray as SmallArray


onBranchElement :: forall a b. Eq a => Int -> (a -> Bool) -> Focus a STM b -> Focus (Branch a) STM b
onBranchElement hash testA aFocus@(Focus concealA revealA) =
  let
    Focus concealLeaves revealLeaves = SmallArray.onFoundElementFocus testA aFocus
    branchesFocus :: Int -> Focus (TVar (SparseSmallArray (Branch a))) STM b
    branchesFocus hash = let
      ssaIndex = HashAccessors.index hash
      in onTVarValue (SparseSmallArray.onElementAtFocus ssaIndex (branchFocus hash))
    branchFocus :: Int -> Focus (Branch a) STM b
    branchFocus hash = Focus concealBranch revealBranch where
      Focus concealBranchesVar revealBranchesVar = branchesFocus (HashConstructors.succLevel hash)
      concealBranch = fmap (fmap (fmap (LeavesBranch hash))) concealLeaves
      revealBranch = \ case
        LeavesBranch leavesHash leavesArray -> case leavesHash == hash of
          True -> fmap (fmap (fmap (LeavesBranch leavesHash))) (revealLeaves leavesArray)
          False -> concealA >>= traverse interpretChange where
            interpretChange = \ case
              Set newA -> let
                newHash = HashConstructors.succLevel hash
                newLeavesHash = HashConstructors.succLevel leavesHash
                in Set <$> BranchConstructors.pair newHash (BranchConstructors.singleton newHash newA) newLeavesHash (LeavesBranch newLeavesHash leavesArray)
              _ -> return Leave
        BranchesBranch (Hamt var) -> fmap (fmap (fmap (BranchesBranch . Hamt))) (revealBranchesVar var)
    in branchFocus hash

onHamtElement :: Eq a => Int -> (a -> Bool) -> Focus a STM b -> Focus (Hamt a) STM b
onHamtElement hash test =
  let
    ssaIndex = HashAccessors.index hash
    in
      mappingInput Hamt (\ (Hamt x) -> x) .
      onTVarValue .
      SparseSmallArray.onElementAtFocus ssaIndex .
      onBranchElement hash test
