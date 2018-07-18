{-|
Utility focuses.
-}
module StmHamt.Focuses where

import StmHamt.Prelude
import StmHamt.Types
import Focus
import qualified StmHamt.Accessors.Hash as HashAccessors
import qualified StmHamt.Accessors.Indices as IndicesAccessors
import qualified StmHamt.Accessors.SmallArray as SmallArrayAccessors
import qualified StmHamt.Constructors.Branch as BranchConstructors
import qualified StmHamt.Constructors.Hash as HashConstructors
import qualified StmHamt.Constructors.Indices as IndicesConstructors
import qualified StmHamt.Constructors.SmallArray as SmallArrayConstructors


onSparseSmallArrayElement :: Monad m => Int -> Focus a m b -> Focus (SparseSmallArray a) m b
onSparseSmallArrayElement index (Focus concealA revealA) = Focus concealSsa revealSsa where
  concealSsa = fmap (fmap aChangeToSsaChange) concealA where
    aChangeToSsaChange = \ case
      Leave -> Leave
      Set a -> Set (SparseSmallArray (IndicesConstructors.singleton index) (pure a))
      Remove -> Leave
  revealSsa (SparseSmallArray indices array) =
    fmap (fmap aChangeToSsaChange) $
    if IndicesAccessors.elem index indices 
      then do
        a <- indexSmallArrayM array (IndicesAccessors.position index indices)
        revealA a
      else concealA
    where
      aChangeToSsaChange = \ case
        Leave -> Leave
        Set a -> if IndicesAccessors.elem index indices
          then let
            newArray = SmallArrayConstructors.set index a array
            in Set (SparseSmallArray indices newArray)
          else let
            newIndices = IndicesConstructors.insert index indices
            newArray = SmallArrayConstructors.insert index a array
            in Set (SparseSmallArray newIndices newArray)
        Remove -> let
          newIndices = IndicesConstructors.invert index indices
          in if IndicesAccessors.null newIndices
            then Remove
            else let
              newArray = SmallArrayConstructors.unset index array
              in Set (SparseSmallArray newIndices newArray)

onFoundSmallArrayElement :: Monad m => (a -> Bool) -> Focus a m b -> Focus (SmallArray a) m b
onFoundSmallArrayElement testA (Focus concealA revealA) = Focus concealArray revealArray where
  concealArray = fmap (fmap arrayChange) concealA where
    arrayChange = \ case
      Set newValue -> Set (pure newValue)
      _ -> Leave
  revealArray array = case SmallArrayAccessors.findWithIndex testA array of
    Just (index, value) -> fmap (fmap arrayChange) (revealA value) where
      arrayChange = \ case
        Leave -> Leave
        Set newValue -> Set (SmallArrayConstructors.set index newValue array)
        Remove -> if sizeofSmallArray array > 1
          then Set (SmallArrayConstructors.unset index array)
          else Remove
    Nothing -> fmap (fmap arrayChange) concealA where
      arrayChange = \ case
        Set newValue -> Set (SmallArrayConstructors.cons newValue array)
        _ -> Leave

onBranchElement :: forall a b. Hash -> (a -> Bool) -> Focus a STM b -> Focus (Branch a) STM b
onBranchElement hash testA aFocus@(Focus concealA revealA) =
  let
    Focus concealLeaves revealLeaves = onFoundSmallArrayElement testA aFocus
    branchesFocus :: Hash -> Focus (TVar (SparseSmallArray (Branch a))) STM b
    branchesFocus hash = let
      ssaIndex = HashAccessors.index hash
      in onTVarValue (onSparseSmallArrayElement ssaIndex (branchFocus hash))
    branchFocus :: Hash -> Focus (Branch a) STM b
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

onHamtElement :: Hash -> (a -> Bool) -> Focus a STM b -> Focus (Hamt a) STM b
onHamtElement hash test =
  let
    ssaIndex = HashAccessors.index hash
    in
      mappingInput Hamt (\ (Hamt x) -> x) .
      onTVarValue .
      onSparseSmallArrayElement ssaIndex .
      onBranchElement hash test
