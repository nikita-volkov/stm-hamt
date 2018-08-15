{-|
Utility focuses.
-}
module StmHamt.Focuses where

import StmHamt.Prelude
import StmHamt.Types
import Focus
import qualified StmHamt.IntOps as IntOps
import qualified StmHamt.Constructors.Branch as BranchConstructors
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray
import qualified PrimitiveExtras.SmallArray as SmallArray


onBranchElement :: forall a b. Int -> Int -> (a -> Bool) -> Focus a STM b -> Focus (Branch a) STM b
onBranchElement depth hash testA aFocus@(Focus concealA revealA) =
  let
    Focus concealLeaves revealLeaves = SmallArray.onFoundElementFocus testA (const False) aFocus
    branchesFocus :: Int -> Focus (TVar (SparseSmallArray (Branch a))) STM b
    branchesFocus depth = let
      !branchIndex = IntOps.indexAtDepth depth hash
      in onTVarValue (SparseSmallArray.onElementAtFocus branchIndex (branchFocus hash))
    branchFocus :: Int -> Focus (Branch a) STM b
    branchFocus depth = Focus concealBranch revealBranch where
      Focus concealBranchesVar revealBranchesVar = branchesFocus (IntOps.nextDepth depth)
      concealBranch = fmap (fmap (fmap (LeavesBranch hash))) concealLeaves
      revealBranch = \ case
        LeavesBranch leavesHash leavesArray -> case leavesHash == hash of
          True -> fmap (fmap (fmap (LeavesBranch leavesHash))) (revealLeaves leavesArray)
          False -> concealA >>= traverse interpretChange where
            interpretChange = \ case
              Set newA -> Set <$> BranchConstructors.pair (IntOps.nextDepth depth) hash (BranchConstructors.singleton hash newA) hash (LeavesBranch hash leavesArray)
              _ -> return Leave
        BranchesBranch (Hamt var) -> fmap (fmap (fmap (BranchesBranch . Hamt))) (revealBranchesVar var)
    in branchFocus hash

onHamtElement :: Int -> Int -> (a -> Bool) -> Focus a STM b -> Focus (Hamt a) STM b
onHamtElement depth hash test focus =
  let
    !branchIndex = IntOps.indexAtDepth depth hash
    Focus concealBranches revealBranches =
      SparseSmallArray.onElementAtFocus branchIndex $
      onBranchElement (IntOps.nextDepth depth) hash test focus
    concealHamt = concealBranches >>= traverse hamtChangeStm where
      hamtChangeStm = \ case
        Leave -> return Leave
        Set !branches -> Set . Hamt <$> newTVar branches
        Remove -> Set . Hamt <$> newTVar SparseSmallArray.empty
    revealHamt (Hamt branchesVar) = do
      branches <- readTVar branchesVar
      (result, branchesChange) <- revealBranches branches
      case branchesChange of
        Leave -> return (result, Leave)
        Set !newBranches -> writeTVar branchesVar newBranches $> (result, Leave)
        Remove -> writeTVar branchesVar SparseSmallArray.empty $> (result, Leave)
    in Focus concealHamt revealHamt
