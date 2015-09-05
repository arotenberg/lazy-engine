module LazyEngine.OperationalToGMachine(
    operationalToGMachine
) where

import Control.Arrow(second)
import Control.Monad(liftM)
import Data.List(foldl')
import qualified Data.Map as Map
import Data.Maybe(catMaybes)

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(Expr(..), CasePat(..), Term(..))

operationalToGMachine :: O.Module -> G.Module
operationalToGMachine (O.Module dataDecls supercombinators) = G.Module gDataDecls gSupercombinators
  where gDataDecls = Map.map compileDataDecl dataDecls
        gSupercombinators = Map.map compileSupercombinator supercombinators

compileDataDecl :: O.DataDecl -> G.DataDecl
compileDataDecl (O.DataDecl ctors) = G.DataDecl ctors

compileSupercombinator :: O.Supercombinator -> G.Supercombinator
compileSupercombinator (O.Supercombinator args body) = G.Supercombinator (length args) bodyInstrs
  where bodyInstrs = prependInstrs (Env Map.empty Map.empty)
            (pushArgsInstrs ++ updateRootToHoleInstrs) constructGraphInstrs
        (env, pushArgsInstrs) = supercombinatorArgsSetup args
        -- If the supercombinator body evaluates anything recursively, convert the redex root to a
        -- black hole immediately after popping all the arguments to avoid space leaks and allow
        -- infinite loop detection.
        updateRootToHoleInstrs
            | any isEval constructGraphInstrsList = [PushRedexRoot, UpdateTo HoleCell]
            | otherwise = []
        constructGraphInstrs = compileExpr 0 env body
        constructGraphInstrsList = concatMap snd constructGraphInstrs

supercombinatorArgsSetup :: [Maybe VarID] -> (Env, [G.Instruction])
supercombinatorArgsSetup args = (env, pushArgsInstrs)
  where namedArgs = catMaybes args
        env = Env (Map.fromList $ zip namedArgs [0..]) Map.empty
        
        pushArgsInstrs = concatMap (uncurry pushArgInstrs) $ zip argLocals args
        
        argLocals = scanl updateArgNumber 0 args
        updateArgNumber n Nothing  = n
        updateArgNumber n (Just _) = n + 1
        
        pushArgInstrs _ Nothing  = [GetArg, Pop]
        pushArgInstrs n (Just _) = [GetArg, PopLocal n]

isEval :: G.Instruction -> Bool
isEval Eval = True
isEval _ = False

-- Env normalLocals noEscapeLocals
data Env = Env !(Map.Map VarID Int) !(Map.Map VarID NoEscapeInfo)
    deriving (Show)
-- NoEscapeInfo label firstNewLocal
-- Fields are not strict since we actually use the laziness in compileExpr.
data NoEscapeInfo = NoEscapeInfo G.Label Int
    deriving (Show)

localsCount :: Env -> Int
localsCount (Env normalLocals _) = Map.size normalLocals

addNormalVarToEnv :: VarID -> Env -> (Int, Env)
addNormalVarToEnv var env@(Env normalLocals noEscapeLocals) =
    (localIndex, Env normalLocals' noEscapeLocals')
  where localIndex = localsCount env
        normalLocals' = Map.insert var localIndex normalLocals
        -- Make sure we don't end up with a normal and no-escape local with the same name!
        noEscapeLocals' = Map.delete var noEscapeLocals

addNoEscapeVarToEnv :: VarID -> NoEscapeInfo -> Env -> Env
addNoEscapeVarToEnv var noEscapeInfo (Env normalLocals noEscapeLocals) =
    Env normalLocals' noEscapeLocals'
  where noEscapeLocals' = Map.insert var noEscapeInfo noEscapeLocals
        normalLocals' = Map.delete var normalLocals

isFree :: VarID -> Env -> Bool
isFree var (Env normalLocals noEscapeLocals) =
    Map.notMember var normalLocals && Map.notMember var noEscapeLocals

prependInstrs :: Env -> [G.Instruction] -> [(Int, [G.Instruction])] ->
    [(Int, [G.Instruction])]
prependInstrs env newCode ((_, firstCode) : rest) =
    (localsCount env, newCode ++ firstCode) : rest
prependInstrs _ _ [] = error "Empty list on right-hand side of prependInstrs."

compileExpr :: G.Label -> Env -> Expr -> [(Int, [G.Instruction])]
compileExpr blockNum env (Let var e1 e2) =
    prependInstrs env (c env e1 ++ [PopLocal localIndex]) (compileExpr blockNum env' e2)
  where (localIndex, env') = addNormalVarToEnv var env
compileExpr blockNum env@(Env normalLocals noEscapeLocals) (LetRec defs e2) =
    prependInstrs env
    (concat [[MakeHole, PopLocal i] | i <- [prevLocalsCount..prevLocalsCount + Map.size defs - 1]] ++
    concat [PushLocal (localIndices Map.! var) : c' env' e1 | (var, e1) <- Map.toList defs])
    (compileExpr blockNum env' e2)
  where prevLocalsCount = localsCount env
        localIndices = Map.fromList $ zip (Map.keys defs) [prevLocalsCount..]
        normalLocals' = localIndices `Map.union` normalLocals  -- Note left-biased union.
        noEscapeLocals' = noEscapeLocals `Map.difference` localIndices
        env' = Env normalLocals' noEscapeLocals'
compileExpr blockNum env (LetNoEscape defs e2) = defsCode
  where -- Use laziness to reference the label numbers before they have been computed.
        env' = foldl' addNoEscapeVar env (Map.keys defs)
        addNoEscapeVar e var =
            addNoEscapeVarToEnv var (NoEscapeInfo (defsBranches Map.! var) firstNewLocal) e
        firstNewLocal = localsCount env
        allDefs = ([], e2) : Map.elems defs
        defsCodeScan = scanl addDef (blockNum, []) allDefs
        addDef (n, bs) (params, expr) =
            let blockEnv = foldl' (\e param -> snd (addNormalVarToEnv param e)) env' params
                exprCode = compileExpr n blockEnv expr
            in (n + length exprCode, bs ++ exprCode)
        (_, defsCode) = last defsCodeScan
        defsBranches = Map.fromList $ zip (Map.keys defs) $ map fst $ tail defsCodeScan
compileExpr blockNum env (Case scrutinee scrutineeVar cases defaultCase) =
    (localsCount env, caseJumpCode) : casesCode
  where caseJumpCode = ee env scrutinee ++ [Dup, PopLocal localIndex,
            caseJumpInstr caseBranches defaultCaseBranch]
        (localIndex, env') = addNormalVarToEnv scrutineeVar env
        -- Put the default case first, since doing so increases the chances that the peephole
        -- optimizations will be able to eliminate an unconditional goto instruction.
        allCases = defaultCase : Map.elems cases
        defaultCaseBranch = blockNum + 1
        casesCodeScan = scanl addCase (defaultCaseBranch, []) allCases
        addCase (n, bs) e =
            let eCode = compileExpr n env' e
            in (n + length eCode, bs ++ eCode)
        (_, casesCode) = last casesCodeScan
        caseBranches = Map.fromList $ zip (Map.keys cases) $ map fst $ tail casesCodeScan
compileExpr _ env (TermExpr e) = [(localsCount env, compileTerm env e)]

caseJumpInstr :: Map.Map CasePat G.Label -> G.Label -> Instruction
caseJumpInstr caseBranches defaultCaseBranch =
    case Map.minViewWithKey caseBranches of
        Nothing -> GoTo defaultCaseBranch
        Just ((p, _), _) -> case p of
            CtorPat _ ->
                let ctorBranches = Map.mapKeys (\(CtorPat n) -> n) caseBranches
                in CtorCaseJump ctorBranches defaultCaseBranch
            IntPat _ ->
                let intBranches = Map.mapKeys (\(IntPat n) -> n) caseBranches
                in IntCaseJump intBranches defaultCaseBranch

-- | @compileTerm env e destCode@ is G-code that updates the destination node on top of the stack
-- to a graph of @e@, evaluating @e@ to WHNF unless doing so would cause tail calls to be replaced
-- with stack-consuming calls.
compileTerm :: Env -> Term -> [G.Instruction]
compileTerm env@(Env _ noEscapeLocals) e = case unrollFuncAp e of
    Just (f, args)
        | Just (NoEscapeInfo label firstNewLocal) <- Map.lookup f noEscapeLocals ->
            -- We assume that the number of arguments given is correct. Note that the argument
            -- graphs are constructed first, then popped out to locals, rather than populating the
            -- locals as the graphs are constructed. This is because some of the parameter locals
            -- may share indices with locals that are currently in scope and interleaving the
            -- construction and popping could corrupt the resulting graphs.
            concatMap (c env) (reverse args) ++
                map PopLocal [firstNewLocal..firstNewLocal + length args - 1] ++ [GoTo label]
        | [lhs, rhs] <- args, isFree f env, Just op <- Map.lookup f binaryOps ->
            wrapRootTerm (ee env rhs ++ ee env lhs ++ [BinaryIntOp op, UpdateTo IndirectionCell])
    _ -> wrapRootTerm (c' env e)

wrapRootTerm :: [G.Instruction] -> [G.Instruction]
wrapRootTerm eCode = [PushRedexRoot] ++ eCode ++ [Unwind]

-- | @c env e@ is G-code that pushes a graph of @e@ onto the stack.
c :: Env -> Term -> [G.Instruction]
c _   (IntLiteral value) = [MakeBoxedInt value]
c (Env normalLocals _) (Var var)
    | Just i <- var `Map.lookup` normalLocals = [PushLocal i]
    | otherwise = [PushGlobal var]
c _   (Ctor typeName ctorName) = [PushNoArgsCtor typeName ctorName]
c env (f `Ap` x) = [MakeHole, Dup] ++ c env f ++ c env x ++ [UpdateTo ApCell]

-- | @c' env e destCode@ is G-code that updates the destination node on top of the stack to a
-- graph of @e@.
c' :: Env -> Term -> [G.Instruction]
c' env (f `Ap` x) = c env f ++ c env x ++ [UpdateTo ApCell]
c' env e = c env e ++ [UpdateTo IndirectionCell]

-- | @ee env e@ is G-code that evaluates @e@ to WHNF and pushes the result onto the stack.
ee :: Env -> Term -> [G.Instruction]
ee _   (IntLiteral value) = [MakeBoxedInt value]
ee env (Var opName `Ap` lhs `Ap` rhs)
    | isFree opName env, Just op <- Map.lookup opName binaryOps =
        ee env rhs ++ ee env lhs ++ [BinaryIntOp op]
ee _   (Ctor typeName ctorName) = [PushNoArgsCtor typeName ctorName]
ee env e = c env e ++ [Eval]

unrollFuncAp :: Term -> Maybe (VarID, [Term])
unrollFuncAp t = second reverse `liftM` unrollFuncAp' t
unrollFuncAp' :: Term -> Maybe (VarID, [Term])
unrollFuncAp' (Var name) = Just (name, [])
unrollFuncAp' (Ctor _ _) = Nothing
unrollFuncAp' (IntLiteral _) = Nothing
unrollFuncAp' (lhs `Ap` rhs) = second (rhs :) `liftM` unrollFuncAp lhs

binaryOps :: Map.Map VarID G.BinaryOp
binaryOps = Map.fromList [
    (VarID "plusInt",   G.PlusOp),
    (VarID "minusInt",  G.MinusOp),
    (VarID "timesInt",  G.TimesOp),
    (VarID "quotInt",   G.QuotOp),
    (VarID "remInt",    G.RemOp)
  ]
