module LazyEngine.Operational(
    module LazyEngine.Name,
    Module(..),
    DataDecl(..),
    Supercombinator(..),
    Expr(..),
    CasePat(..),
    Term(..),
    local,
    global
) where

import Data.Int
import qualified Data.Map as Map

import LazyEngine.Name

data Module = Module (Map.Map TypeName DataDecl) (Map.Map GlobalName Supercombinator)
    deriving (Show)
data DataDecl = DataDecl [CtorName]
    deriving (Show)
data Supercombinator = Supercombinator [Maybe LocalID] Expr
    deriving (Show)

-- | The body of a supercombinator.
data Expr
    = TermExpr Term
    | Let LocalID Term Expr
    | LetRec (Map.Map LocalID Term) Expr
    | LetNoEscape (Map.Map LocalID ([LocalID], Expr)) Expr
    | Case Term LocalID (Map.Map CasePat Expr) Expr
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
