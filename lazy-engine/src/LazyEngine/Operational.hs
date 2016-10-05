module LazyEngine.Operational(
    module LazyEngine.Name,
    Module(..),
    Decl(..),
    Expr(..),
    LetNoEscapeBinding(..),
    CasePat(..),
    Term(..),
    Value(..),
    local,
    global
) where

import Data.Int

import LazyEngine.Name

data Module = Module [Decl]
    deriving (Show)
data Decl
    = DataDecl TypeName [CtorDecl]
    | GlobalDecl GlobalName [LocalID] Expr
  deriving (Show)

-- | The body of a global variable declaration.
data Expr
    = Return Value
    | Let LocalID Term Expr
    | LetRec [(LocalID, Term)] Expr
    | LetNoEscape [(LocalID, LetNoEscapeBinding)] Expr
    | CallLNE LocalID [Value]
    | Case Value LocalID [(CasePat, Expr)] Expr
    | EvalBinaryOp BinaryOp Value Value LocalID Expr
  deriving (Show)
data LetNoEscapeBinding = LetNoEscapeBinding [LocalID] Expr
    deriving (Show)
data CasePat
    = CtorPat TypeName CtorName [LocalID]
    | IntPat Int32
  deriving (Show, Eq, Ord)

-- | A "constructible" expression that has a direct representation as cells in memory.
data Term
    = ValueTerm Value
    -- | A saturated data constructor application.
    | Ctor TypeName CtorName [Term]
    | IntLiteral Int32
    | Term `Ap` Term
  deriving (Show)
infixl 9 `Ap`

-- | An expression whose representation as cells in memory can be constructed without any additional
-- heap allocation.
data Value
    = Local LocalID
    | Global GlobalName
  deriving (Show)

local :: Int -> Value
local = Local . LocalID

global :: String -> Value
global = Global . GlobalName
