module LazyEngine.OperationalToGMachineTests(tests) where

import qualified Data.Map as Map
import Test.HUnit

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import LazyEngine.OperationalToGMachine
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(Expr(..), Term(..))

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
    TestCase $ assertEqual "assertion" expected (operationalToGMachine input)

testEmpty = operationalToGMachineTest expected input
  where input = O.Module Map.empty Map.empty
        expected = G.Module Map.empty Map.empty

testId = operationalToGMachineTest expected input
  where input = O.Module Map.empty $ Map.singleton (VarID "id") $
            O.Supercombinator [Just (VarID "x")] $ TermExpr $ Var (VarID "x")
        expected = G.Module Map.empty $
            Map.singleton (VarID "id") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            PushRedexRoot,
            PushLocal 0,
            UpdateTo IndirectionCell,
            Unwind
          ])]

testConst = operationalToGMachineTest expected input
  where input = O.Module Map.empty $ Map.singleton (VarID "const") $
            O.Supercombinator [Just (VarID "x"), Nothing] $ TermExpr $ Var (VarID "x")
        expected = G.Module Map.empty $
            Map.singleton (VarID "const") (G.Supercombinator 2 expectedInstrs)
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
  where input = O.Module Map.empty $ Map.singleton (VarID "fix") $
            O.Supercombinator [Just (VarID "f")] $ TermExpr $
                Var (VarID "f") `Ap` (Var (VarID "fix") `Ap` Var (VarID "f"))
        expected = G.Module Map.empty $
            Map.singleton (VarID "fix") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            PushRedexRoot,
            PushLocal 0,
            MakeHole,
            Dup,
            PushGlobal (VarID "fix"),
            PushLocal 0,
            UpdateTo ApCell,
            UpdateTo ApCell,
            Unwind
          ])]

-- TODO: This test demonstrates a case that could be made slightly more efficient. If the redex
-- root ends up being set to a variable from an immediately-enclosing letrec, we can update the
-- redex root itself in-place instead of allocating a new cell and setting the redex root to be an
-- indirection to that. The corresponding situation with a let instead of a letrec should never
-- happen because the let will be inlined away by earlier stages.
testFixKnotTying = operationalToGMachineTest expected input
  where input = O.Module Map.empty $ Map.singleton (VarID "fix") $
            O.Supercombinator [Just (VarID "f")] $
                LetRec (Map.singleton (VarID "x") $ Var (VarID "f") `Ap` Var (VarID "x")) $
                TermExpr $ Var (VarID "x")
        expected = G.Module Map.empty $
            Map.singleton (VarID "fix") (G.Supercombinator 1 expectedInstrs)
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
  where input = O.Module Map.empty $ Map.singleton (VarID "foo") $
            O.Supercombinator [] $ TermExpr $ IntLiteral 3
        expected = G.Module Map.empty $
            Map.singleton (VarID "foo") (G.Supercombinator 0 expectedInstrs)
        expectedInstrs = [(0, [
            PushRedexRoot,
            MakeBoxedInt 3,
            UpdateTo IndirectionCell,
            Unwind
          ])]
