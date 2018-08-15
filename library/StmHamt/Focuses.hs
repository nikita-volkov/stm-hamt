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


onBranchCases :: Int -> Focus (Hamt a) STM b -> Focus (SmallArray a) STM b -> Focus (Branch a) STM b
onBranchCases hash (Focus concealHamt revealHamt) (Focus concealElementArray revealElementArray) =
  Focus
    (let
      interpretChange = \ case
        Set !hamt -> return (Set (BranchesBranch hamt))
        _ -> return Leave
      in concealHamt >>= traverse interpretChange)
    (\ case
      BranchesBranch hamt -> let
        interpretChange = \ case
          Set !newHamt -> return (Set (BranchesBranch hamt))
          Leave -> return Leave
          Remove -> return Remove
        in revealHamt hamt >>= traverse interpretChange
      LeavesBranch leavesHash leafArray -> fmap (fmap (fmap (LeavesBranch leavesHash)))
        (if leavesHash == hash
          then revealElementArray leafArray
          else concealElementArray))

onHamtBranch :: Int -> Focus (Branch a) STM b -> Focus (Hamt a) STM b
onHamtBranch index =
  mappingInput Hamt (\ (Hamt var) -> var) .
  onTVarValue .
  SparseSmallArray.onElementAtFocus index

onBranchElement :: Int -> Int -> (a -> Bool) -> Focus a STM b -> Focus (Branch a) STM b
onBranchElement depth hash testElement elementFocus = loop depth where
  loop depth =
    onBranchCases hash
      (onHamtBranch (IntOps.indexAtDepth depth hash) (loop (IntOps.nextDepth depth)))
      (SmallArray.onFoundElementFocus testElement (const False) elementFocus)

onHamtElement :: Int -> (a -> Bool) -> Focus a STM b -> Focus (Hamt a) STM b
onHamtElement hash testElement elementFocus = loop 0 where
  loop depth =
    onHamtBranch (IntOps.indexAtDepth depth hash)
      (onBranchCases hash
        (loop (IntOps.nextDepth depth))
        (SmallArray.onFoundElementFocus testElement (const False) elementFocus))
