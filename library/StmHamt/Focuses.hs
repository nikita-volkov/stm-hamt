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
onBranchElement depth hash testElement elementFocus@(Focus concealElement revealElement) =
  let
    ~(Focus concealLeaves revealLeaves) = SmallArray.onFoundElementFocus testElement (const False) elementFocus
    branchesVarFocus :: Int -> Focus (TVar (SparseSmallArray (Branch a))) STM b
    branchesVarFocus depth = let
      !branchIndex = IntOps.indexAtDepth depth hash
      in onTVarValue (SparseSmallArray.onElementAtFocus branchIndex (branchFocus ( depth)))
    branchFocus :: Int -> Focus (Branch a) STM b
    branchFocus depth = Focus concealBranch revealBranch where
      concealBranch = fmap (fmap (fmap (LeavesBranch hash))) concealLeaves
      revealBranch = \ case
        LeavesBranch leavesHash leavesArray -> 
          case leavesHash == hash of
            True -> fmap (fmap (fmap (LeavesBranch leavesHash))) (revealLeaves leavesArray)
            False -> let
              interpretChange = \ case
                Set !newElement -> Set <$> BranchConstructors.pair (IntOps.nextDepth depth) hash (BranchConstructors.singleton hash newElement) leavesHash (LeavesBranch leavesHash leavesArray)
                _ -> return Leave
              in concealElement >>= traverse interpretChange
        BranchesBranch (Hamt var) -> let
          Focus _ revealBranchesVar = branchesVarFocus (IntOps.nextDepth depth)
          in fmap (fmap (fmap (BranchesBranch . Hamt))) (revealBranchesVar var)
    in branchFocus depth

onHamtElement :: Int -> Int -> (a -> Bool) -> Focus a STM b -> Focus (Hamt a) STM b
onHamtElement depth hash test focus =
  let
    branchIndex = IntOps.indexAtDepth depth hash
    Focus concealBranches revealBranches =
      SparseSmallArray.onElementAtFocus branchIndex $
      onBranchElement depth hash test focus
    concealHamt = let
      hamtChangeStm = \ case
        Leave -> return Leave
        Set !branches -> Set . Hamt <$> newTVar branches
        Remove -> Set . Hamt <$> newTVar SparseSmallArray.empty
      in concealBranches >>= traverse hamtChangeStm
    revealHamt (Hamt branchesVar) = do
      branches <- readTVar branchesVar
      (result, branchesChange) <- revealBranches branches
      case branchesChange of
        Leave -> return (result, Leave)
        Set !newBranches -> writeTVar branchesVar newBranches $> (result, Leave)
        Remove -> writeTVar branchesVar SparseSmallArray.empty $> (result, Leave)
    in Focus concealHamt revealHamt
