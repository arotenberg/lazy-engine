module LazyEngine.GMachine where

import Data.Int
import qualified Data.Map as Map

import LazyEngine.Name

-- The Eq instances are only present for the unit tests.

data Module = Module (Map.Map TyVarID DataDecl) (Map.Map VarID Supercombinator)
    deriving (Show, Eq)

data DataDecl = DataDecl [VarID]
    deriving (Show, Eq)

data Supercombinator = Supercombinator Int [(Int, [Instruction])]
    deriving (Show, Eq)

type Label = Int
data Instruction
    = Unwind                 -- s  -->  [empty]  (unwind from redex root)
    | GetArg                 -- s  -->  argValue : s  (argValue is removed from arg stack)
    | PushRedexRoot          -- s  -->  redexRoot : s
    | PushGlobal VarID       -- s  -->  globalValue : s
    | PushNoArgsCtor TyVarID VarID  -- s  -->  globalValue : s
    | PushLocal Int          -- s  -->  localValue : s
    | Pop                    -- value : s  -->  s  (discard value)
    | PopLocal  Int          -- value : s  -->  s  (set local to point to value)
    | Dup                    -- value : s  -->  value : value : s
    | MakeHole               -- s  -->  newCell : s
    | MakeBoxedInt Int32     -- s  -->  newNode : s
    | UpdateTo CellContent   -- args ++ dest : s  -->  s  (dest is updated)
    | Eval                   -- value : s  -->  evaluatedValue : s
        -- (evaluatedValue is equivalent to value, is in WHNF, and is not an indirection node)
    | BinaryIntOp BinaryOp   -- lhs : rhs : s  -->  result : s
        -- (lhs and rhs are boxed int nodes; result is a newly-allocated boxed int node)
    | CtorCaseJump (Map.Map Int32 Label) Label
        -- value : s  -->  s  (value is an algebraic data type node; jump to selected label)
    | IntCaseJump (Map.Map Int32 Label) Label
        -- value : s  -->  s  (value is a boxed int node; jump to selected label)
    | GoTo Label  -- s  -->  s  (jump to specified label)
  deriving (Show, Eq)
data CellContent
    = HoleCell             -- args = []
    | IndirectionCell      -- args = [value]
    | ApCell               -- args = [x, f]
  deriving (Show, Eq, Ord)
data BinaryOp = PlusOp | MinusOp | TimesOp | QuotOp | RemOp
    deriving (Show, Eq, Ord)
