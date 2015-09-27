module LazyEngine.Operational(
    module LazyEngine.Name,
    Module(..),
    Decl(..),
    Expr(..),
    LetNoEscapeBinding(..),
    CasePat(..),
    Term(..),
    local,
    global
) where

import Data.Int

import LazyEngine.Name

data Module = Module [Decl]
    deriving (Show)
data Decl
    = DataDecl TypeName [CtorName]
    | GlobalDecl GlobalName [LocalID] Expr
  deriving (Show)

-- | The body of a global variable declaration.
data Expr
    = TermExpr Term
    | Let LocalID Term Expr
    | LetRec [(LocalID, Term)] Expr
    | LetNoEscape [(LocalID, LetNoEscapeBinding)] Expr
    | Case Term LocalID [(CasePat, Expr)] Expr
  deriving (Show)
data LetNoEscapeBinding = LetNoEscapeBinding [LocalID] Expr
    deriving (Show)
data CasePat
    = CtorPat Int32
    | IntPat Int32
  deriving (Show, Eq, Ord)
-- | A "constructible" expression that has a direct representation as cells in-memory.
data Term
    = Local LocalID
    | Global GlobalName
    -- | A saturated data constructor application.
    | Ctor TypeName CtorName
    | IntLiteral Int32
    | Term `Ap` Term
  deriving (Show)
infixl `Ap`

local :: Int -> Term
local = Local . LocalID

global :: String -> Term
global = Global . GlobalName
