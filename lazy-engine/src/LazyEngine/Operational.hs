module LazyEngine.Operational where

import Data.Int
import qualified Data.Map as Map

import LazyEngine.Name

data Module = Module (Map.Map TyVarID DataDecl) (Map.Map VarID Supercombinator)
    deriving (Show)

data DataDecl = DataDecl [VarID]
    deriving (Show)

data Supercombinator = Supercombinator [Maybe VarID] Expr
    deriving (Show)

-- | The body of a supercombinator.
data Expr
    = TermExpr Term
    | Let VarID Term Expr
    | LetRec (Map.Map VarID Term) Expr
    | LetNoEscape (Map.Map VarID ([VarID], Expr)) Expr
    | Case Term VarID (Map.Map CasePat Expr) Expr
  deriving (Show)
data CasePat
    = CtorPat Int32
    | IntPat Int32
  deriving (Show, Eq, Ord)
-- | A "constructible" expression that has a direct representation as cells in-memory.
data Term
    = Var VarID
    -- | A saturated data constructor application.
    | Ctor TyVarID VarID
    | IntLiteral Int32
    | Term `Ap` Term
  deriving (Show)
infixl `Ap`

var :: String -> Term
var = Var . VarID
