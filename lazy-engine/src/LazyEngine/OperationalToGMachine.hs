module LazyEngine.OperationalToGMachine(
    ConversionError,
    operationalToGMachine
) where

import Data.List(foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(Expr(..), LetNoEscapeBinding(..), CasePat(..), Term(..))
import LazyEngine.OperationalChecker
import LazyEngine.OperationalUtils

operationalToGMachine :: O.Module -> Either ConversionError G.Module
operationalToGMachine m = do
    checkOperational m
    return (compileModule m)

compileModule :: O.Module -> G.Module
compileModule (O.Module decls) = G.Module gDataDecls gSupercombinators
  where gDataDecls = Map.fromList [(typeName, G.DataDecl ctors) |
            O.DataDecl typeName ctors <- decls]
        gSupercombinators = Map.fromList [(globalName, compileSupercombinator args body) |
            O.GlobalDecl globalName args body <- decls]

compileSupercombinator :: [LocalID] -> O.Expr -> G.Supercombinator
compileSupercombinator args body = G.Supercombinator (length args) bodyInstrs
  where -- It's safe to just tack entry instructions onto the first block of instructions like this
        -- because neither case nor let-no-escape can ever jump back to the first block.
        bodyInstrs = prependInstrs (Env Map.empty Map.empty)
            (pushArgsInstrs ++ updateRootToHoleInstrs) constructGraphInstrs
        (env, pushArgsInstrs) = supercombinatorArgsSetup args (freeLocals body)
        -- If the supercombinator body evaluates anything recursively, convert the redex root to a
        -- black hole immediately after popping all the arguments to avoid space leaks and allow
        -- infinite loop detection.
        updateRootToHoleInstrs
            | any isEval constructGraphInstrsList = [PushRedexRoot, UpdateTo HoleCell]
            | otherwise = []
        constructGraphInstrs = compileExpr 0 env body
        constructGraphInstrsList = concatMap snd constructGraphInstrs

isEval :: G.Instruction -> Bool
isEval Eval = True
isEval _ = False

supercombinatorArgsSetup :: [LocalID] -> Set.Set LocalID -> (Env, [G.Instruction])
supercombinatorArgsSetup args bodyFreeLocals = (env, pushArgsInstrs)
  where argsUsed = map (`Set.member` bodyFreeLocals) args
        usedArgs = filter (`Set.member` bodyFreeLocals) args
        env = Env (Map.fromList $ zip usedArgs [0..]) Map.empty
        
        pushArgsInstrs = concatMap (uncurry pushArgInstrs) $ zip argLocals argsUsed
        
        argLocals = scanl updateArgNumber 0 argsUsed
        updateArgNumber n False = n
        updateArgNumber n True  = n + 1
        
        pushArgInstrs _ False = [GetArg, Pop]
        pushArgInstrs n True  = [GetArg, PopLocal n]

-- | @Env normalLocals noEscapeLocals@
data Env = Env !(Map.Map LocalID Int) !(Map.Map LocalID NoEscapeInfo)
    deriving (Show)
-- | @NoEscapeInfo label firstNewLocal@
-- Fields are not strict since we actually use the laziness in 'compileExpr'.
data NoEscapeInfo = NoEscapeInfo G.Label Int
    deriving (Show)

localsCount :: Env -> Int
localsCount (Env normalLocals _) = Map.size normalLocals

addNormalVarToEnv :: LocalID -> Env -> (Int, Env)
addNormalVarToEnv var env@(Env normalLocals noEscapeLocals) =
    (localIndex, Env normalLocals' noEscapeLocals)
  where localIndex = localsCount env
        normalLocals' = Map.insert var localIndex normalLocals

addNoEscapeVarToEnv :: LocalID -> NoEscapeInfo -> Env -> Env
addNoEscapeVarToEnv var noEscapeInfo (Env normalLocals noEscapeLocals) =
    Env normalLocals noEscapeLocals'
  where noEscapeLocals' = Map.insert var noEscapeInfo noEscapeLocals

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
    (concat [[MakeHole, PopLocal i] | i <- [prevLocalsCount..prevLocalsCount + length defs - 1]] ++
    concat [PushLocal (localIndices Map.! var) : c' env' e1 | (var, e1) <- defs])
    (compileExpr blockNum env' e2)
  where prevLocalsCount = localsCount env
        localIndices = Map.fromList $ zip (map fst defs) [prevLocalsCount..]
        normalLocals' = localIndices `Map.union` normalLocals  -- Note left-biased union.
        noEscapeLocals' = noEscapeLocals `Map.difference` localIndices
        env' = Env normalLocals' noEscapeLocals'
compileExpr blockNum env (LetNoEscape defs e2) = defsCode
  where -- Use laziness to reference the label numbers before they have been computed.
        env' = foldl' addNoEscapeVar env (map fst defs)
        addNoEscapeVar e var =
            addNoEscapeVarToEnv var (NoEscapeInfo (defsBranches Map.! var) firstNewLocal) e
        firstNewLocal = localsCount env
        allDefs = LetNoEscapeBinding [] e2 : map snd defs
        defsCodeScan = scanl addDef (blockNum, []) allDefs
        addDef (n, bs) (LetNoEscapeBinding params expr) =
            let blockEnv = foldl' (\e param -> snd (addNormalVarToEnv param e)) env' params
                exprCode = compileExpr n blockEnv expr
            in (n + length exprCode, bs ++ exprCode)
        (_, defsCode) = last defsCodeScan
        defsBranches = Map.fromList $ zip (map fst defs) $ map fst $ tail defsCodeScan
compileExpr blockNum env (Case scrutinee scrutineeVar cases defaultCase) =
    (localsCount env, caseJumpCode) : casesCode
  where caseJumpCode = ee env scrutinee ++ [Dup, PopLocal localIndex,
            caseJumpInstr caseBranches defaultCaseBranch]
        (localIndex, env') = addNormalVarToEnv scrutineeVar env
        -- Put the default case first, since doing so increases the chances that the peephole
        -- optimizations will be able to eliminate an unconditional goto instruction.
        allCases = defaultCase : map snd cases
        defaultCaseBranch = blockNum + 1
        casesCodeScan = scanl addCase (defaultCaseBranch, []) allCases
        addCase (n, bs) e =
            let eCode = compileExpr n env' e
            in (n + length eCode, bs ++ eCode)
        (_, casesCode) = last casesCodeScan
        caseBranches = Map.fromList $ zip (map fst cases) $ map fst $ tail casesCodeScan
compileExpr _ env@(Env _ noEscapeLocals) (CallLNE f args) = [(localsCount env, callCode)]
  where -- Note that the argument graphs are constructed first, then popped out to locals, rather
        -- than populating the locals as the graphs are constructed. This is because some of the
        -- parameter locals may share indices with locals that are currently in scope and
        -- interleaving the construction and popping could corrupt the resulting graphs.
        callCode = concatMap (c env) (reverse args) ++
            map PopLocal [firstNewLocal..firstNewLocal + length args - 1] ++ [GoTo label]
        NoEscapeInfo label firstNewLocal = noEscapeLocals Map.! f
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
compileTerm env (Global opName `Ap` lhs `Ap` rhs)
    | Just op <- Map.lookup opName binaryOps =
        wrapRootTerm (ee env rhs ++ ee env lhs ++ [BinaryIntOp op, UpdateTo IndirectionCell])
compileTerm env e = wrapRootTerm (c' env e)

wrapRootTerm :: [G.Instruction] -> [G.Instruction]
wrapRootTerm eCode = [PushRedexRoot] ++ eCode ++ [Unwind]

-- | @c env e@ is G-code that pushes a graph of @e@ onto the stack.
c :: Env -> Term -> [G.Instruction]
c _   (IntLiteral value) = [MakeBoxedInt value]
c (Env normalLocals _) (Local var)
    | Just i <- var `Map.lookup` normalLocals = [PushLocal i]
    | otherwise = error $ "Unknown local: " ++ show var
c _ (Global var) = [PushGlobal var]
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
ee env (Global opName `Ap` lhs `Ap` rhs)
    | Just op <- Map.lookup opName binaryOps =
        ee env rhs ++ ee env lhs ++ [BinaryIntOp op]
ee _   (Ctor typeName ctorName) = [PushNoArgsCtor typeName ctorName]
ee env e = c env e ++ [Eval]

binaryOps :: Map.Map GlobalName G.BinaryOp
binaryOps = Map.fromList [
    (GlobalName "plusInt",   G.PlusOp),
    (GlobalName "minusInt",  G.MinusOp),
    (GlobalName "timesInt",  G.TimesOp),
    (GlobalName "quotInt",   G.QuotOp),
    (GlobalName "remInt",    G.RemOp)
  ]
