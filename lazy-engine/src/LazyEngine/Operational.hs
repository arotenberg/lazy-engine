module LazyEngine.Operational(
    module LazyEngine.Name,
    Module(..),
    DataDecl(..),
    Supercombinator(..),
    Expr(..),
    CasePat(..),
    Term(..),
    local,
    global,
    ExprPart(..)
) where

import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set

import LazyEngine.Name

data Module = Module (Map.Map TypeName DataDecl) (Map.Map GlobalName Supercombinator)
    deriving (Show)
data DataDecl = DataDecl [CtorName]
    deriving (Show)
data Supercombinator = Supercombinator [LocalID] Expr
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

class ExprPart e where
    freeLocals :: e -> Set.Set LocalID
instance ExprPart Expr where
    freeLocals (TermExpr e) = freeLocals e
    freeLocals (Let var e1 e2) = freeLocals e1 `Set.union` Set.delete var (freeLocals e2)
    freeLocals (LetRec defs e) =
        (defsFreeLocals `Set.union` freeLocals e) `Set.difference` Map.keysSet defs
      where defsFreeLocals = Set.unions $ map freeLocals $ Map.elems defs
    freeLocals (LetNoEscape defs e) =
        (defsFreeLocals `Set.union` freeLocals e) `Set.difference` Map.keysSet defs
      where defsFreeLocals = Set.unions $ map freeDefLocals $ Map.elems defs
            freeDefLocals (args, defE) = freeLocals defE `Set.difference` Set.fromList args
    freeLocals (Case scrutinee scrutineeVar cases defaultCase) =
        freeLocals scrutinee `Set.union` Set.delete scrutineeVar allCasesFreeLocals
      where allCasesFreeLocals = casesFreeLocals `Set.union` freeLocals defaultCase
            casesFreeLocals = Set.unions $ map freeLocals $ Map.elems cases
instance ExprPart Term where
    freeLocals (Local var) = Set.singleton var
    freeLocals (Global _) = Set.empty
    freeLocals (Ctor _ _) = Set.empty
    freeLocals (IntLiteral _) = Set.empty
    freeLocals (f `Ap` x) = freeLocals f `Set.union` freeLocals x
