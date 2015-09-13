module LazyEngine.JVM.GMachineToJavaClassFile(
    gMachineToJavaClassFile
) where

import Data.Int(Int32)
import qualified Data.Map as Map

import LazyEngine.GMachine
import LazyEngine.Name

import LazyEngine.JVM.JavaClassFile

gMachineClassName, nodeClassName, adtClassName :: String
gMachineClassName = "org.lazyengine.runtime.GMachine"
nodeClassName = "org.lazyengine.runtime.Node"
cellClassName = "org.lazyengine.runtime.Cell"
adtClassName = "org.lazyengine.runtime.AlgebraicDataType"
boxedIntClassName = "org.lazyengine.runtime.BoxedInt"

gMachineToJavaClassFile :: String -> Module -> [JClass]
gMachineToJavaClassFile mainClassName (Module dataDecls globals) = mainClass : dataDeclClasses
  where mainClass = moduleMainClass mainClassName globals
        dataDeclClasses = [dataDeclClass mainClassName typeName dataDecl |
            (typeName, dataDecl) <- Map.toList dataDecls]

dataDeclClass :: String -> TyVarID -> DataDecl -> JClass
dataDeclClass mainClassName (TyVarID typeName) (DataDecl ctors) = JClass {
    jClassName = dataClassName,
    jClassSuperclassName = adtClassName,
    jClassPublic = False,
    jClassFinality = JFinal,
    jClassFields = map sharedNodeField ctors,
    jClassMethods = [dataStaticInitializerMethod dataClassName ctors, dataClassCtor dataClassName]
  }
  where dataClassName = mainClassName ++ "$" ++ typeName

dataStaticInitializerMethod :: String -> [VarID] -> JMethod
dataStaticInitializerMethod dataClassName ctors =
    staticInitializerMethod [(JStackMapFrame [] [], compiledInstrs)]
  where compiledInstrs = concat [initCtorInstrs dataClassName ordinal ctor |
            (ordinal, ctor) <- zip [0..] ctors] ++ [JInstr_return]

initCtorInstrs :: String -> Int32 -> VarID -> [JInstr]
initCtorInstrs dataClassName ordinal (VarID ctorName) = [
    JInstr_new dataClassName,
    JInstr_dup,
    JInstr_ldc (StringValue ctorName),
    JInstr_ldc (IntValue ordinal),
    invokeCtor dataClassName [JObjectType "java.lang.String", JPrimitiveType JInt],
    JInstr_putstatic dataClassName ("shared$" ++ ctorName) (JObjectType nodeClassName)
  ]

dataClassCtor :: String -> JMethod
dataClassCtor dataClassName = JMethod {
    jMethodName = "<init>",
    jMethodVisibility = JPrivate,
    jMethodStatic = False,
    jMethodFinality = JOverridable,
    jMethodStrictfp = False,
    jMethodSignature = JSignature [JObjectType "java.lang.String", JPrimitiveType JInt] Nothing,
    jMethodCodeBlocks = dataClassCtorInstrs dataClassName
  }

dataClassCtorInstrs :: String -> [(JStackMapFrame, [JInstr])]
dataClassCtorInstrs dataClassName = [(JStackMapFrame initialLocals [], [
    JInstr_aload 0,
    JInstr_aload 1,
    JInstr_iload 2,
    invokeCtor adtClassName [JObjectType "java.lang.String", JPrimitiveType JInt],
    JInstr_return
  ])]
  where initialLocals = [VT_object dataClassName, VT_object "java.lang.String", VT_int]

moduleMainClass :: String -> Map.Map VarID Supercombinator -> JClass
moduleMainClass mainClassName globals = JClass {
    jClassName = mainClassName,
    jClassSuperclassName = "java.lang.Object",
    jClassPublic = False,
    jClassFinality = JFinal,
    jClassFields = map sharedNodeField (Map.keys globals),
    jClassMethods = moduleStaticInitializerMethod mainClassName globals :
        [compileSupercombinator mainClassName name instrs
            | (name, Supercombinator _ instrs) <- Map.toList globals]
  }

sharedNodeField :: VarID -> JField
sharedNodeField (VarID name) = JField {
    jFieldName = "shared$" ++ name,
    jFieldVisibility = JPackagePrivate,
    jFieldStatic = True,
    jFieldFinal = True,
    jFieldType = JObjectType nodeClassName
  }

moduleStaticInitializerMethod :: String -> Map.Map VarID Supercombinator -> JMethod
moduleStaticInitializerMethod mainClassName globals =
    staticInitializerMethod [(JStackMapFrame [] [], compiledInstrs)]
  where compiledInstrs = concat [initSupercombinatorInstrs mainClassName name argCount |
            (name, Supercombinator argCount _) <- Map.toList globals] ++ [JInstr_return]

initSupercombinatorInstrs :: String -> VarID -> Int -> [JInstr]
initSupercombinatorInstrs mainClassName (VarID name) argCount = [
    JInstr_ldc $ StringValue name,
    JInstr_ldc $ IntValue (fromIntegral argCount),
    JInstr_ldc $ MethodHandle_invokestatic_Value mainClassName name
        (JSignature [JObjectType gMachineClassName, JObjectType cellClassName] Nothing),
    JInstr_invoke Static nodeClassName "makeSupercombinator"
        (JSignature [JObjectType "java.lang.String", JPrimitiveType JInt,
            JObjectType "java.lang.invoke.MethodHandle"]
        (Just $ JObjectType nodeClassName)),
    JInstr_putstatic mainClassName ("shared$" ++ name) (JObjectType nodeClassName)
  ]

compileSupercombinator :: String -> VarID -> [(Int, [Instruction])] -> JMethod
compileSupercombinator mainClassName (VarID name) instrs = JMethod {
    jMethodName = name,
    jMethodVisibility = JPrivate,
    jMethodStatic = True,
    jMethodFinality = JOverridable,
    jMethodStrictfp = False,
    jMethodSignature = JSignature [JObjectType gMachineClassName,
        JObjectType cellClassName] Nothing,
    jMethodCodeBlocks = compileInstructions mainClassName instrs
  }

compileInstructions :: String -> [(Int, [Instruction])] -> [(JStackMapFrame, [JInstr])]
compileInstructions mainClassName instrs = zip stackMap optimizedInstrs
  where optimizedInstrs = peepholeOptimize $ map (concatMap c . snd) instrs
        c = compileInstruction mainClassName
        stackMap = map (buildStackMapFrame . fst) instrs

buildStackMapFrame :: Int -> JStackMapFrame
buildStackMapFrame nodeLocalsCount = JStackMapFrame locals []
  where locals = [VT_object gMachineClassName, VT_object cellClassName] ++ nodeLocals
        nodeLocals = replicate nodeLocalsCount (VT_object nodeClassName)

compileInstruction :: String -> Instruction -> [JInstr]
compileInstruction _ Unwind = [JInstr_return]
compileInstruction _ GetArg = [
    JInstr_aload 0,
    JInstr_invoke Virtual gMachineClassName "popArg"
        (JSignature [] (Just $ JObjectType nodeClassName))
  ]
compileInstruction _ PushRedexRoot = [JInstr_aload 1]
compileInstruction mainClassName (PushGlobal (VarID name)) =
    [JInstr_getstatic mainClassName ("shared$" ++ name) (JObjectType nodeClassName)]
compileInstruction mainClassName (PushNoArgsCtor (TyVarID typeName) (VarID ctorName)) =
    [JInstr_getstatic dataClassName ("shared$" ++ ctorName) (JObjectType nodeClassName)]
  where dataClassName = mainClassName ++ "$" ++ typeName
compileInstruction _ (PushLocal index) = [JInstr_aload (fromIntegral (2 + index))]
compileInstruction _ Pop = [JInstr_pop]
compileInstruction _ (PopLocal index) = [JInstr_astore (fromIntegral (2 + index))]
compileInstruction _ Dup = [JInstr_dup]
compileInstruction _ MakeHole =
    [JInstr_invoke Static cellClassName "makeHole"
        (JSignature [] (Just $ JObjectType cellClassName))]
compileInstruction _ (MakeBoxedInt value) = [
    JInstr_ldc (IntValue value),
    JInstr_invoke Static boxedIntClassName "valueOf"
        (JSignature [JPrimitiveType JInt] (Just $ JObjectType boxedIntClassName))
  ]
compileInstruction _ (UpdateTo HoleCell) =
    [JInstr_invoke Virtual cellClassName "updateToHole" $ JSignature [] Nothing]
compileInstruction _ (UpdateTo IndirectionCell) =
    [JInstr_invoke Virtual cellClassName "updateToIndirection" $
        JSignature [JObjectType nodeClassName] Nothing]
compileInstruction _ (UpdateTo ApCell) =
    [JInstr_invoke Virtual cellClassName "updateToAp" $
        JSignature [JObjectType nodeClassName, JObjectType nodeClassName] Nothing]
compileInstruction _ Eval = [
    JInstr_aload 0,
    JInstr_swap,
    JInstr_invoke Virtual gMachineClassName "eval"
        (JSignature [JObjectType nodeClassName] (Just $ JObjectType nodeClassName))
  ]
compileInstruction _ (BinaryIntOp op) = [
    JInstr_checkcast boxedIntClassName,
    JInstr_invoke Virtual boxedIntClassName "getValue"
        (JSignature [] (Just $ JPrimitiveType JInt)),
    JInstr_swap,
    JInstr_checkcast boxedIntClassName,
    JInstr_invoke Virtual boxedIntClassName "getValue"
        (JSignature [] (Just $ JPrimitiveType JInt)),
    intBinaryOpInstr op,
    JInstr_invoke Static boxedIntClassName "valueOf"
        (JSignature [JPrimitiveType JInt] (Just $ JObjectType boxedIntClassName))
  ]
compileInstruction _ (CtorCaseJump cases defaultCase)
    | Map.null cases = [JInstr_pop, JInstr_goto defaultCase]
    | otherwise = [
        JInstr_checkcast adtClassName,
        JInstr_invoke Virtual adtClassName "ordinal"
            (JSignature [] (Just $ JPrimitiveType JInt))
      ] ++ buildSwitchInstrs cases defaultCase
compileInstruction _ (IntCaseJump cases defaultCase)
    | Map.null cases = [JInstr_pop, JInstr_goto defaultCase]
    | otherwise = [
        JInstr_checkcast boxedIntClassName,
        JInstr_invoke Virtual boxedIntClassName "getValue"
            (JSignature [] (Just $ JPrimitiveType JInt))
      ] ++ buildSwitchInstrs cases defaultCase
compileInstruction _ (GoTo label) = [JInstr_goto label]

intBinaryOpInstr :: BinaryOp -> JInstr
intBinaryOpInstr PlusOp   = JInstr_iadd
intBinaryOpInstr MinusOp  = JInstr_isub
intBinaryOpInstr TimesOp  = JInstr_imul
intBinaryOpInstr QuotOp   = JInstr_idiv
intBinaryOpInstr RemOp    = JInstr_irem
