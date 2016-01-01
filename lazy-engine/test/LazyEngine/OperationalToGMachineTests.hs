module LazyEngine.OperationalToGMachineTests(tests) where

import qualified Data.Map as Map
import Test.HUnit

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import LazyEngine.OperationalToGMachine
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(
    Expr(..), LetNoEscapeBinding(..), CasePat(..), Term(..), global, local)

tests :: Test
tests = TestList [
    TestLabel "testEmpty" testEmpty,
    TestLabel "testId" testId,
    TestLabel "testConst" testConst,
    TestLabel "testFixGlobal" testFixGlobal,
    TestLabel "testFixKnotTying" testFixKnotTying,
    TestLabel "testIntLiteral" testIntLiteral,
    TestLabel "testLetNoEscapeLoop" testLetNoEscapeLoop,
    TestLabel "testLetNoEscapeFactorial" testLetNoEscapeFactorial
  ]

operationalToGMachineTest :: G.Module -> O.Module -> Test
operationalToGMachineTest expected input =
    TestCase $ assertEqual "assertion" (Right expected) (operationalToGMachine input)

testEmpty = operationalToGMachineTest expected input
  where input = O.Module []
        expected = G.Module Map.empty Map.empty

testId = operationalToGMachineTest expected input
  where input = O.Module [O.GlobalDecl (GlobalName "id") [LocalID 1] $
            Return $ local 1]
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
  where input = O.Module [O.GlobalDecl (GlobalName "const") [LocalID 1, LocalID 2] $
            Return $ local 1]
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
  where input = O.Module [O.GlobalDecl (GlobalName "fix") [LocalID 1] $
            Let (LocalID 2) (
                ValueTerm (local 1) `Ap` (ValueTerm (global "fix") `Ap` ValueTerm (local 1))) $
            Return $ local 2]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "fix") (G.Supercombinator 1 expectedInstrs)
        expectedInstrs = [(0, [
            GetArg,
            PopLocal 0,
            MakeHole,
            Dup,
            PushLocal 0,
            MakeHole,
            Dup,
            PushGlobal (GlobalName "fix"),
            PushLocal 0,
            UpdateTo ApCell,
            UpdateTo ApCell,
            PopLocal 1,
            PushRedexRoot,
            PushLocal 1,
            UpdateTo IndirectionCell,
            Unwind
          ])]

-- TODO: This test demonstrates a case that could be made slightly more efficient. If the redex
-- root ends up being set to a variable from an immediately-enclosing let(rec), we can update the
-- redex root itself in-place instead of allocating a new cell and setting the redex root to be an
-- indirection to that.
testFixKnotTying = operationalToGMachineTest expected input
  where input = O.Module [O.GlobalDecl (GlobalName "fix") [LocalID 1] $
            LetRec [(LocalID 2, ValueTerm (local 1) `Ap` ValueTerm (local 2))] $
            Return $ local 2]
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
  where input = O.Module [O.GlobalDecl (GlobalName "foo") [] $
            Let (LocalID 1) (IntLiteral 3) $
            Return $ local 1]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "foo") (G.Supercombinator 0 expectedInstrs)
        expectedInstrs = [(0, [
            MakeBoxedInt 3,
            PopLocal 0,
            PushRedexRoot,
            PushLocal 0,
            UpdateTo IndirectionCell,
            Unwind
          ])]

testLetNoEscapeLoop = operationalToGMachineTest expected input
  where input = O.Module [O.GlobalDecl (GlobalName "loop") [] $
            LetNoEscape [(LocalID 1, LetNoEscapeBinding [] $ CallLNE (LocalID 1) [])] $
                CallLNE (LocalID 1) []]
        expected = G.Module Map.empty $
            Map.singleton (GlobalName "loop") (G.Supercombinator 0 expectedInstrs)
        expectedInstrs = [
            (0, [
                GoTo 1
            ]),
            (0, [
                GoTo 1
            ])]

testLetNoEscapeFactorial = operationalToGMachineTest expected input
  where input = O.Module [
            O.GlobalDecl (GlobalName "one") [] $
                Let (LocalID 1) (IntLiteral 1) $
                Return (local 1),
            O.GlobalDecl (GlobalName "factorial") [LocalID 1] $
                LetNoEscape [(LocalID 2, LetNoEscapeBinding [LocalID 3, LocalID 4] $
                    Case (local 4) (LocalID 5) [(IntPat 0, Return $ local 3)] $
                        EvalBinaryOp TimesOp (local 3) (local 5) (LocalID 6) $
                            EvalBinaryOp MinusOp (local 4) (global "one") (LocalID 7) $
                                CallLNE (LocalID 2) [local 6, local 7])] $
                    CallLNE (LocalID 2) [global "one", local 1]
          ]
        expected = G.Module Map.empty $ Map.fromList [
            (GlobalName "one", G.Supercombinator 0 expectedOneInstrs),
            (GlobalName "factorial", G.Supercombinator 1 expectedFactorialInstrs)
          ]
        expectedOneInstrs = [
            (0, [
                MakeBoxedInt 1,
                PopLocal 0,
                PushRedexRoot,
                PushLocal 0,
                UpdateTo IndirectionCell,
                Unwind
            ])
          ]
        expectedFactorialInstrs = [
            (0, [
                GetArg,
                PopLocal 0,
                PushRedexRoot,
                UpdateTo HoleCell,
                PushLocal 0,
                PushGlobal (GlobalName "one"),
                PopLocal 1,
                PopLocal 2,
                GoTo 1
            ]),
            (3, [
                PushLocal 2,
                Eval,
                Dup,
                PopLocal 3,
                IntCaseJump (Map.singleton 0 3) 2
            ]),
            (4, [
                PushLocal 3,
                Eval,
                PushLocal 1,
                Eval,
                BinaryIntOp TimesOp,
                PopLocal 4,
                PushGlobal (GlobalName "one"),
                Eval,
                PushLocal 2,
                Eval,
                BinaryIntOp MinusOp,
                PopLocal 5,
                PushLocal 5,
                PushLocal 4,
                PopLocal 1,
                PopLocal 2,
                GoTo 1
            ]),
            (4, [
                PushRedexRoot,
                PushLocal 1,
                UpdateTo IndirectionCell,
                Unwind
            ])]
