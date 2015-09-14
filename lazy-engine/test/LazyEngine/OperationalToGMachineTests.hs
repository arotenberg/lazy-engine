module LazyEngine.OperationalToGMachineTests(tests) where

import qualified Data.Map as Map
import Test.HUnit

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import LazyEngine.OperationalToGMachine
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(Expr(..), Term(..), global, local)

tests :: Test
tests = TestList [
    TestLabel "testEmpty" testEmpty,
    TestLabel "testId" testId,
    TestLabel "testConst" testConst,
    TestLabel "testFixGlobal" testFixGlobal,
    TestLabel "testFixKnotTying" testFixKnotTying,
    TestLabel "testIntLiteral" testIntLiteral
  ]

operationalToGMachineTest :: G.Module -> O.Module -> Test
operationalToGMachineTest expected input =
    TestCase $ assertEqual "assertion" (Right expected) (operationalToGMachine input)

testEmpty = operationalToGMachineTest expected input
  where input = O.Module [] []
        expected = G.Module Map.empty Map.empty

testId = operationalToGMachineTest expected input
  where input = O.Module [] [(GlobalName "id",
            O.Supercombinator [LocalID 1] $ TermExpr $ local 1)]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "id") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            PushRedexRoot,
            PushLocal 0,
            UpdateTo IndirectionCell,
            Unwind
          ])]

testConst = operationalToGMachineTest expected input
  where input = O.Module [] [(GlobalName "const",
            O.Supercombinator [LocalID 1, LocalID 2] $ TermExpr $ local 1)]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "const") (G.Supercombinator 2 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            GetArg,
            Pop,
            PushRedexRoot,
            PushLocal 0,
            UpdateTo IndirectionCell,
            Unwind
          ])]

testFixGlobal = operationalToGMachineTest expected input
  where input = O.Module [] [(GlobalName "fix",
            O.Supercombinator [LocalID 1] $ TermExpr $ local 1 `Ap` (global "fix" `Ap` local 1))]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "fix") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            PushRedexRoot,
            PushLocal 0,
            MakeHole,
            Dup,
            PushGlobal (GlobalName "fix"),
            PushLocal 0,
            UpdateTo ApCell,
            UpdateTo ApCell,
            Unwind
          ])]

-- TODO: This test demonstrates a case that could be made slightly more efficient. If the redex
-- root ends up being set to a variable from an immediately-enclosing let(rec), we can update the
-- redex root itself in-place instead of allocating a new cell and setting the redex root to be an
-- indirection to that. The client can always optimize let expressions away so that this situation
-- does not happen, but, as this case demonstrates, this is not always possible for letrecs.
testFixKnotTying = operationalToGMachineTest expected input
  where input = O.Module [] [(GlobalName "fix", 
            O.Supercombinator [LocalID 1] $
                LetRec (Map.singleton (LocalID 2) $ local 1 `Ap` local 2) $ TermExpr $ local 2)]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "fix") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            MakeHole,
            PopLocal 1,
            PushLocal 1,
            PushLocal 0,
            PushLocal 1,
            UpdateTo ApCell,
            PushRedexRoot,
            PushLocal 1,
            UpdateTo IndirectionCell,
            Unwind
          ])]

testIntLiteral = operationalToGMachineTest expected input
  where input = O.Module [] [(GlobalName "foo", O.Supercombinator [] $ TermExpr $ IntLiteral 3)]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "foo") (G.Supercombinator 0 expectedInstrs)
        expectedInstrs = [(0, [
            PushRedexRoot,
            MakeBoxedInt 3,
            UpdateTo IndirectionCell,
            Unwind
          ])]
