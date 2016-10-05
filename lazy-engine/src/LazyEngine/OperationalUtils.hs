module LazyEngine.OperationalUtils(
    ExprPart(..),
    localVariablesDeclaredIn
) where

import qualified Data.Set as Set

import LazyEngine.Operational

class ExprPart e where
    freeLocals :: e -> Set.Set LocalID
instance ExprPart Expr where
    freeLocals (Return e) = freeLocals e
    freeLocals (Let var e1 e2) = freeLocals e1 `Set.union` Set.delete var (freeLocals e2)
    freeLocals (LetRec defs e) =
        (defsFreeLocals `Set.union` freeLocals e) `Set.difference` boundLocals
      where defsFreeLocals = Set.unions $ map (freeLocals . snd) defs
            boundLocals = Set.fromList (map fst defs)
    freeLocals (LetNoEscape defs e) =
        (defsFreeLocals `Set.union` freeLocals e) `Set.difference` boundLocals
      where defsFreeLocals = Set.unions $ map (freeDefLocals . snd) defs
            freeDefLocals (LetNoEscapeBinding args defE) =
                freeLocals defE `Set.difference` Set.fromList args
            boundLocals = Set.fromList (map fst defs)
    freeLocals (CallLNE f args) = Set.insert f $ Set.unions (map freeLocals args)
    freeLocals (Case scrutinee scrutineeVar cases defaultCase) =
        freeLocals scrutinee `Set.union` Set.delete scrutineeVar allCasesFreeLocals
      where allCasesFreeLocals = casesFreeLocals `Set.union` freeLocals defaultCase
            casesFreeLocals = Set.unions $ map (freeLocals . snd) cases
    freeLocals (EvalBinaryOp _ arg1 arg2 var e) =
        freeLocals arg1 `Set.union` freeLocals arg2 `Set.union` Set.insert var (freeLocals e)
instance ExprPart Term where
    freeLocals (ValueTerm value) = freeLocals value
    freeLocals (Ctor _ _ args) = Set.unions (map freeLocals args)
    freeLocals (IntLiteral _) = Set.empty
    freeLocals (f `Ap` x) = freeLocals f `Set.union` freeLocals x
instance ExprPart Value where
    freeLocals (Local var) = Set.singleton var
    freeLocals (Global _) = Set.empty

localVariablesDeclaredIn :: Expr -> [LocalID]
localVariablesDeclaredIn (Return _) = []
localVariablesDeclaredIn (Let var _ e2) = var : localVariablesDeclaredIn e2
localVariablesDeclaredIn (LetRec decls e) = map fst decls ++ localVariablesDeclaredIn e
localVariablesDeclaredIn (LetNoEscape decls e) =
    concatMap declVars decls ++ localVariablesDeclaredIn e
  where declVars (var, LetNoEscapeBinding params body) =
            var : params ++ localVariablesDeclaredIn body
localVariablesDeclaredIn (CallLNE _ _) = []
localVariablesDeclaredIn (Case _ scrutineeVar cases defaultCase) =
    scrutineeVar : concatMap caseVars cases ++ localVariablesDeclaredIn defaultCase
  where caseVars (_, body) = localVariablesDeclaredIn body
localVariablesDeclaredIn (EvalBinaryOp _ _ _ var e) = var : localVariablesDeclaredIn e
