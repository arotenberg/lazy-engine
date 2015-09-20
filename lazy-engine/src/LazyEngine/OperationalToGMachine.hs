module LazyEngine.OperationalToGMachine(
    ConversionError,
    operationalToGMachine
) where

import Control.Arrow(second)
import Control.Monad
import Data.List(foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified LazyEngine.GMachine as G
import LazyEngine.GMachine(Instruction(..), CellContent(..))
import LazyEngine.Name
import qualified LazyEngine.Operational as O
import LazyEngine.Operational(Expr(..), CasePat(..), Term(..))

type ConversionError = String
type Convert = Either ConversionError

conversionError :: ConversionError -> Convert a
conversionError = Left

operationalToGMachine :: O.Module -> Either ConversionError G.Module
operationalToGMachine (O.Module decls) = do
    foldM_ addDeclToGlobalEnv emptyGlobalEnv decls
    let gDataDecls = Map.fromList [(typeName, compileDataDecl ctors) |
            O.DataDecl typeName ctors <- decls]
        gSupercombinators = Map.fromList [(globalName, compileSupercombinator args body) |
            O.GlobalDecl globalName args body <- decls]
    return $ G.Module gDataDecls gSupercombinators

-- | @GlobalEnv dataDecls globalDecls@
data GlobalEnv = GlobalEnv !(Set.Set TypeName) !(Set.Set GlobalName)
    deriving (Show)

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv = GlobalEnv Set.empty Set.empty

addDeclToGlobalEnv :: GlobalEnv -> O.Decl -> Convert GlobalEnv
addDeclToGlobalEnv (GlobalEnv dataDecls globalDecls) (O.DataDecl typeName ctors)
    | typeName `Set.member` dataDecls =
        conversionError $ "Duplicate data type: " ++ show typeName
    | otherwise = do
        foldM_ addCtorToSet Set.empty ctors
        return $ GlobalEnv (Set.insert typeName dataDecls) globalDecls
addDeclToGlobalEnv (GlobalEnv dataDecls globalDecls) (O.GlobalDecl globalName _ _)
    | globalName `Set.member` globalDecls =
        conversionError $ "Duplicate supercombinator name: " ++ show globalName
    | otherwise = return $ GlobalEnv dataDecls (Set.insert globalName globalDecls)

addCtorToSet :: Set.Set CtorName -> CtorName -> Convert (Set.Set CtorName)
addCtorToSet ctorNames ctorName
    | ctorName `Set.member` ctorNames =
        conversionError $ "Duplicate data constructor name: " ++ show ctorName
    | otherwise = return $ Set.insert ctorName ctorNames

compileDataDecl :: [CtorName] -> G.DataDecl
compileDataDecl ctors = G.DataDecl ctors

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
  where argsUsed = argsUsedInBody args bodyFreeLocals
        usedArgs = [arg | (arg, ArgUsed) <- zip args argsUsed]
        env = Env (Map.fromList $ zip usedArgs [0..]) Map.empty
        
        pushArgsInstrs = concatMap (uncurry pushArgInstrs) $ zip argLocals argsUsed
        
        argLocals = scanl updateArgNumber 0 argsUsed
        updateArgNumber n ArgUnused = n
        updateArgNumber n ArgUsed   = n + 1
        
        pushArgInstrs _ ArgUnused = [GetArg, Pop]
        pushArgInstrs n ArgUsed   = [GetArg, PopLocal n]

data ArgUsage = ArgUnused | ArgUsed
    deriving (Show)

argsUsedInBody :: [LocalID] -> Set.Set LocalID -> [ArgUsage]
argsUsedInBody args bodyFreeLocals = fst $ foldr handleArg ([], bodyFreeLocals) args
  where handleArg arg (usedArgs, freeLocs) = (usage : usedArgs, Set.delete arg freeLocs)
          where usage = if arg `Set.member` freeLocs then ArgUsed else ArgUnused

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
    (localIndex, Env normalLocals' noEscapeLocals')
  where localIndex = localsCount env
        normalLocals' = Map.insert var localIndex normalLocals
        -- Make sure we don't end up with a normal and no-escape local with the same name!
        noEscapeLocals' = Map.delete var noEscapeLocals

addNoEscapeVarToEnv :: LocalID -> NoEscapeInfo -> Env -> Env
addNoEscapeVarToEnv var noEscapeInfo (Env normalLocals noEscapeLocals) =
    Env normalLocals' noEscapeLocals'
  where noEscapeLocals' = Map.insert var noEscapeInfo noEscapeLocals
        normalLocals' = Map.delete var normalLocals

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
    (f, reversedArgs)
        | Local v <- f, Just (NoEscapeInfo label firstNewLocal) <- Map.lookup v noEscapeLocals ->
            -- We assume that the number of arguments given is correct. Note that the argument
            -- graphs are constructed first, then popped out to locals, rather than populating the
            -- locals as the graphs are constructed. This is because some of the parameter locals
            -- may share indices with locals that are currently in scope and interleaving the
            -- construction and popping could corrupt the resulting graphs.
            concatMap (c env) reversedArgs ++
                map PopLocal [firstNewLocal..firstNewLocal + length reversedArgs - 1] ++
                [GoTo label]
        | Global g <- f, [rhs, lhs] <- reversedArgs, Just op <- Map.lookup g binaryOps ->
            wrapRootTerm (ee env rhs ++ ee env lhs ++ [BinaryIntOp op, UpdateTo IndirectionCell])
    _ -> wrapRootTerm (c' env e)

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

unrollFuncAp :: Term -> (Term, [Term])
unrollFuncAp (f `Ap` x) = second (x :) (unrollFuncAp f)
unrollFuncAp t = (t, [])

binaryOps :: Map.Map GlobalName G.BinaryOp
binaryOps = Map.fromList [
    (GlobalName "plusInt",   G.PlusOp),
    (GlobalName "minusInt",  G.MinusOp),
    (GlobalName "timesInt",  G.TimesOp),
    (GlobalName "quotInt",   G.QuotOp),
    (GlobalName "remInt",    G.RemOp)
  ]
