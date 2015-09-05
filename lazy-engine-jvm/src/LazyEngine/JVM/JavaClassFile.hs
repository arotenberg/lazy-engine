module LazyEngine.JVM.JavaClassFile(
    JClass(..), JField(..), JMethod(..), jMethodCode,
    JSignature(..), JType(..), JPrimitiveType(..),
    JVisibility(..), JFinality(..),
    JBlockLabel,
    JStackMapFrame(..), JVerificationType(..),
    JInstr(..), JCmp(..), JInvokeType(..), JConstValue(..),
    staticInitializerMethod, invokeCtor, buildSwitchInstrs, peepholeOptimize
) where

import Data.Int
import Data.List(intersperse)
import qualified Data.Map as Map
import Data.Word

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS delim = concatS . intersperse delim

data JClass = JClass {
    jClassName :: String,
    jClassSuperclassName :: String,
    jClassPublic :: Bool,
    jClassFinality :: JFinality,
    jClassFields :: [JField],
    jClassMethods :: [JMethod]
  }
instance Show JClass where
    showsPrec _ c = showPublic . showFinality . showString "class " .
        showString (jClassName c) . showString " extends " . showString (jClassSuperclassName c) .
        showString " {" . concatS (map showMember (jClassFields c)) .
        concatS (map showMember (jClassMethods c)) . showString "}"
      where showPublic = if jClassPublic c then showString "public " else id
            showFinality = showString (finalityString (jClassFinality c))
            showMember m = showString "\n" . shows m . showString "\n"

data JField = JField {
    jFieldName :: String,
    jFieldVisibility :: JVisibility,
    jFieldStatic :: Bool,
    jFieldFinal :: Bool,
    jFieldType :: JType
  }
instance Show JField where
    showsPrec _ f = showString "\t" . showVisibility . showStatic . showFinal .
        showsType (jFieldType f) . showString " " . showString (jFieldName f) . showString ";"
      where showVisibility = showString (visibilityString (jFieldVisibility f))
            showStatic = if jFieldStatic f then showString "static " else id
            showFinal  = if jFieldFinal  f then showString "final "  else id

data JMethod = JMethod {
    jMethodName :: String,
    jMethodVisibility :: JVisibility,
    jMethodStatic :: Bool,
    jMethodFinality :: JFinality,
    jMethodStrictfp :: Bool,
    jMethodSignature :: JSignature,
    jMethodCodeBlocks :: [(JStackMapFrame, [JInstr])]
  }
instance Show JMethod where
    showsPrec _ m = showString "\t" . showVisibility . showStatic . showFinality . showStrictfp .
        showReturnType . showString " " . showString (jMethodName m) . showString "(" .
        showParams . showString ") {" . showsCodeBlocks (jMethodCodeBlocks m) . showString "\t}"
      where showVisibility = showString (visibilityString (jMethodVisibility m))
            showFinality = showString (finalityString (jMethodFinality m))
            showStatic   = if jMethodStatic   m then showString "static "   else id
            showStrictfp = if jMethodStrictfp m then showString "strictfp " else id
            showReturnType = case jSignatureReturnType (jMethodSignature m) of
                Nothing -> showString "void"
                Just t -> showsType t
            showParams = intercalateS (showString ", ") $ map showsType $
                jSignatureParamTypes (jMethodSignature m)

showsCodeBlocks :: [(JStackMapFrame, [JInstr])] -> ShowS
showsCodeBlocks blocks = showString "\t\t" . concatS (map showCodeBlock blocks)
  where showCodeBlock (stackFrame, instrs) = showString "\n\t\t/* " . shows stackFrame .
            showString " */\n" . concatS (map showInstr instrs)
        showInstr i = showString "\t\t" . showString (instrString i) . showString "\n"

jMethodCode :: JMethod -> [[JInstr]]
jMethodCode = map snd . jMethodCodeBlocks

data JSignature = JSignature {
    jSignatureParamTypes :: [JType],
    jSignatureReturnType :: Maybe JType
  } deriving (Show, Eq, Ord)

data JType = JPrimitiveType !JPrimitiveType | JObjectType String | JArrayType !JType
    deriving (Show, Eq, Ord)
showsType :: JType -> ShowS
showsType (JPrimitiveType t) = showString (primitiveTypeString t)
showsType (JObjectType name) = showString name
showsType (JArrayType elementType) = showsType elementType . showString "[]"

data JPrimitiveType
    = JByte
    | JChar
    | JDouble
    | JFloat
    | JInt
    | JLong
    | JShort
    | JBoolean
  deriving (Show, Eq, Ord, Bounded, Enum)
primitiveTypeString :: JPrimitiveType -> String
primitiveTypeString JByte    = "byte"
primitiveTypeString JChar    = "char"
primitiveTypeString JDouble  = "double"
primitiveTypeString JFloat   = "float"
primitiveTypeString JInt     = "int"
primitiveTypeString JLong    = "byte"
primitiveTypeString JShort   = "byte"
primitiveTypeString JBoolean = "byte"

data JVisibility = JPrivate | JPackagePrivate | JProtected | JPublic
    deriving (Show, Eq, Ord, Bounded, Enum)
visibilityString :: JVisibility -> String
visibilityString JPrivate        = "private "
visibilityString JPackagePrivate = ""
visibilityString JProtected      = "protected "
visibilityString JPublic         = "public "

data JFinality = JOverridable | JAbstract | JFinal
    deriving (Show, Eq, Ord, Bounded, Enum)
finalityString :: JFinality -> String
finalityString JOverridable = ""
finalityString JAbstract    = "abstract "
finalityString JFinal       = "final "

type JBlockLabel = Int
-- | JStackMapFrame locals stack
data JStackMapFrame = JStackMapFrame [JVerificationType] [JVerificationType]
    deriving (Show)
data JVerificationType
    = VT_top
    | VT_null
    | VT_uninitialized !Word16
    | VT_uninitializedThis
    | VT_int
    | VT_long
    | VT_float
    | VT_double
    | VT_object String
  deriving (Show, Eq, Ord)

staticInitializerMethod :: [(JStackMapFrame, [JInstr])] -> JMethod
staticInitializerMethod codeBlocks = JMethod {
    jMethodName = "<clinit>",
    jMethodVisibility = JPackagePrivate,
    jMethodStatic = True,
    jMethodFinality = JOverridable,
    jMethodStrictfp = False,
    jMethodSignature = JSignature [] Nothing,
    jMethodCodeBlocks = codeBlocks
  }

invokeCtor :: String -> [JType] -> JInstr
invokeCtor className argTypes =
    JInstr_invoke Special className "<init>" (JSignature argTypes Nothing)

data JInstr
    = JInstr_aaload
    | JInstr_aload !Word16
    | JInstr_astore !Word16
    | JInstr_checkcast String
    | JInstr_dup
    | JInstr_dup_x1
    | JInstr_dup_x2
    | JInstr_getstatic String String JType
    | JInstr_goto JBlockLabel
    | JInstr_iadd
    | JInstr_iastore
    | JInstr_idiv
    | JInstr_if !JCmp JBlockLabel
    | JInstr_if_icmp !JCmp JBlockLabel
    | JInstr_iload !Word16
    | JInstr_imul
    | JInstr_invoke !JInvokeType String String JSignature
    | JInstr_irem
    | JInstr_isub
    | JInstr_ldc !JConstValue
    | JInstr_lookupswitch JBlockLabel (Map.Map Int32 JBlockLabel)
    | JInstr_new String
    | JInstr_newarray JPrimitiveType
    | JInstr_pop
    | JInstr_putstatic String String JType
    | JInstr_return
    | JInstr_swap
    | JInstr_tableswitch JBlockLabel !Int32 [JBlockLabel]
  deriving (Show)
instrString :: JInstr -> String
instrString = drop 7 . show

data JCmp = JCmp_eq | JCmp_ne | JCmp_lt | JCmp_ge | JCmp_gt | JCmp_le
    deriving (Show, Eq, Ord, Bounded, Enum)
data JInvokeType = Special | Static | Virtual
    deriving (Show, Eq, Ord, Bounded, Enum)
data JConstValue
    = IntValue !Int32
    | MethodHandle_invokestatic_Value String String JSignature
    | StringValue String
  deriving (Show)

buildSwitchInstrs :: Map.Map Int32 JBlockLabel -> JBlockLabel -> [JInstr]
buildSwitchInstrs branches defaultBranch
    | nlabels == 0 = [JInstr_pop, JInstr_goto defaultBranch]
    | nlabels == 1 && lo == 0 = [JInstr_if JCmp_eq loBranch, JInstr_goto defaultBranch]
    | nlabels == 1 = [JInstr_ldc (IntValue lo), JInstr_if_icmp JCmp_eq loBranch, JInstr_goto defaultBranch]
    | tableCost <= lookupCost =
        let branchForValue v
                | Just b <- Map.lookup v branches = b
                | otherwise = defaultBranch
            branchList = map branchForValue [lo..hi]
        in [JInstr_tableswitch defaultBranch lo branchList]
    | otherwise = [JInstr_lookupswitch defaultBranch branches]
  where nlabels = fromIntegral (Map.size branches) :: Int32
        (lo, loBranch) = Map.findMin branches
        (hi, _) = Map.findMax branches
        -- This logic is based on the method com.sun.tools.javac.jvm.Gen.visitSwitch
        -- in the OpenJDK javac implementation.
        tableCost, lookupCost :: Int64
        tableSpaceCost = 4 + fromIntegral hi - fromIntegral lo + 1  -- words
        tableTimeCost = 3  -- comparisons
        lookupSpaceCost = 3 + 2 * fromIntegral nlabels
        -- TODO: Should lookupTimeCost be based on log(nlabels) due to binary search lookupswitch?
        lookupTimeCost = fromIntegral nlabels
        tableCost = tableSpaceCost + 3 * tableTimeCost
        lookupCost = lookupSpaceCost + 3 * lookupTimeCost

-- | Implements some peephole optimizations on blocks of JInstrs. The instructions are assumed to
-- represent the entire body of a method as in jMethodCode.
peepholeOptimize :: [[JInstr]] -> [[JInstr]]
peepholeOptimize instrs
    -- Iterate until nothing changes.
    | blocksChanged || hasEmptyBlocks = peepholeOptimize nonemptyBlocks
    | otherwise = nonemptyBlocks
  where optimizedBlocks = map (uncurry p) $ zip [0..] instrs
        blocksChanged = any fst optimizedBlocks
        hasEmptyBlocks = any null (map snd optimizedBlocks)
        nonemptyBlocks = removeEmptyBlocks (map snd optimizedBlocks)
        
        p :: JBlockLabel -> [JInstr] -> (Bool, [JInstr])
        p _ [] = (False, [])
        p b [JInstr_goto b']
            | b' == b + 1 = (True, [])
        p b (JInstr_dup : JInstr_pop : rest) = dropInstr (p b rest)
        p b (JInstr_dup : i@(JInstr_astore _) : JInstr_pop : rest) = i *: p b rest
        p b (i : JInstr_pop : rest)
            | isAtomicLoad i = dropInstr (p b rest)
        p b (i1 : i2 : JInstr_swap : rest)
            | isAtomicLoad i1 && isAtomicLoad i2 = i2 *: i1 *: p b rest
        p b (JInstr_aload m : JInstr_astore n : rest)
            | m == n = dropInstr (p b rest)
        p b (i : rest) = i =: p b rest
        
        i =: (changed, rest) = (changed, i : rest)
        i *: (_, rest) = (True, i : rest)
        infixr *:, =:
        dropInstr (_, rest) = (True, rest)

removeEmptyBlocks :: [[JInstr]] -> [[JInstr]]
removeEmptyBlocks blocks = map (map (remapBranchDestinations (labelMap Map.!))) nonemptyBlocks
  where nonemptyBlocks = filter (not . null) blocks
        nonemptyBlockCount = length nonemptyBlocks
        labelMap = Map.fromList (zip [0..] labelList)
        labelList = map (nonemptyBlockCount -) $ init $ scanr buildLabelList 0 blocks
        buildLabelList [] n = n
        buildLabelList _  n = n + 1

isAtomicLoad :: JInstr -> Bool
isAtomicLoad (JInstr_aload _) = True
isAtomicLoad (JInstr_iload _) = True
isAtomicLoad (JInstr_ldc _)   = True
isAtomicLoad _ = False

remapBranchDestinations :: (JBlockLabel -> JBlockLabel) -> JInstr -> JInstr
remapBranchDestinations f (JInstr_goto b) = JInstr_goto (f b)
remapBranchDestinations f (JInstr_if cmp b) = JInstr_if cmp (f b)
remapBranchDestinations f (JInstr_if_icmp cmp b) = JInstr_if_icmp cmp (f b)
remapBranchDestinations f (JInstr_lookupswitch defaultBranch branches) =
    JInstr_lookupswitch (f defaultBranch) (Map.map f branches)
remapBranchDestinations f (JInstr_tableswitch defaultBranch lo branches) =
    JInstr_tableswitch (f defaultBranch) lo (map f branches)
remapBranchDestinations _ i = i
