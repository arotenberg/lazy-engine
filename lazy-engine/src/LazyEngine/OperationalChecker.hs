module LazyEngine.OperationalChecker(
    ConversionError,
    checkOperational
) where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import LazyEngine.Operational
import LazyEngine.OperationalUtils

type ConversionError = String
type Check = Either ConversionError

conversionError :: ConversionError -> Check a
conversionError = Left

checkForDuplicates :: (Ord a, Show a) => String -> [a] -> Check (Set.Set a)
checkForDuplicates elemDesc = foldM addToSet Set.empty
  where addToSet s x
            | x `Set.member` s = conversionError $ "Duplicate " ++ elemDesc ++ ": " ++ show x
            | otherwise = return (Set.insert x s)

checkOperational :: Module -> Either ConversionError ()
checkOperational (Module decls) = do
    globalEnv <- foldM addDeclToGlobalEnv emptyGlobalEnv decls
    forM_ decls (checkDecl globalEnv)

-- | @GlobalEnv dataDecls globalDecls@
data GlobalEnv = GlobalEnv !(Map.Map TypeName (Map.Map CtorName CtorFields)) !(Set.Set GlobalName)
    deriving (Show)

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv = GlobalEnv Map.empty Set.empty

addDeclToGlobalEnv :: GlobalEnv -> Decl -> Check GlobalEnv
addDeclToGlobalEnv (GlobalEnv dataDecls globalDecls) (DataDecl typeName ctors)
    | typeName `Map.member` dataDecls =
        conversionError $ "Duplicate data type: " ++ show typeName
    | otherwise = do
        ctorMap <- foldM addToMap Map.empty ctors
        return $ GlobalEnv (Map.insert typeName ctorMap dataDecls) globalDecls
  where addToMap m (CtorDecl ctorName ctorFields)
            | ctorName `Map.member` m =
                conversionError $ "Duplicate data constructor name: " ++ show ctorName
            | otherwise = return (Map.insert ctorName ctorFields m)
addDeclToGlobalEnv (GlobalEnv dataDecls globalDecls) (GlobalDecl globalName _ _)
    | globalName `Set.member` globalDecls =
        conversionError $ "Duplicate supercombinator name: " ++ show globalName
    | otherwise = return $ GlobalEnv dataDecls (Set.insert globalName globalDecls)

checkDecl :: GlobalEnv -> Decl -> Check ()
checkDecl _ (DataDecl _ _) = return ()
checkDecl globalEnv (GlobalDecl _ params body) = do
    let localDecls = params ++ localVariablesDeclaredIn body
    checkForDuplicates "local variable ID" localDecls
    -- TODO: Check local types, scope, and case patterns.
    return ()
