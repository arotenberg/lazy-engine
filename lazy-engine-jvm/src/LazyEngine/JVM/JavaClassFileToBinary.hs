{-# LANGUAGE BangPatterns #-}

module LazyEngine.JVM.JavaClassFileToBinary(
    javaClassFileToBinary, fileNameFromClassName
) where

import Control.Monad
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State(StateT, execStateT, get, put)
import Data.Binary.Put
import Data.Bits((.|.))
import qualified Data.ByteString.Lazy as B
import Data.Function(on)
import Data.Int
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word

import LazyEngine.JVM.JavaClassFile

javaClassFileToBinary :: JClass -> B.ByteString
javaClassFileToBinary c = runPut finalGen
  where ((constPoolCount, constRefs), afterConstantPoolBytes) =
            runConstantPoolGen (writeSectionsAfterConstantPool c)
        finalGen = do
            putWord32be 0xCAFEBABE  -- magic
            putWord16be 0           -- minor_version
            putWord16be 51          -- major_version
            writeConstantPool constPoolCount constRefs
            putLazyByteString afterConstantPoolBytes

writeSectionsAfterConstantPool :: JClass -> ConstantPoolGen ()
writeSectionsAfterConstantPool c = do
    lift $ putWord16be $ (if jClassPublic c then acc_public else 0) .|.
        finalityToAccessFlags (jClassFinality c) .|. acc_super
    writeConstRef $ ClassConstRef (jClassName c)
    writeConstRef $ ClassConstRef (jClassSuperclassName c)
    lift $ putWord16be 0  -- interfaces_count
    
    lift $ putWord16be $ fromIntegral (length (jClassFields c))
    forM_ (jClassFields c) writeField
    
    lift $ putWord16be $ fromIntegral (length (jClassMethods c))
    forM_ (jClassMethods c) (writeMethod (jClassName c))
    
    lift $ putWord16be 0  -- attributes_count

writeField :: JField -> ConstantPoolGen ()
writeField f = do
    lift $ putWord16be $ visibilityToAccessFlags (jFieldVisibility f) .|.
        (if jFieldStatic f then acc_static else 0) .|. (if jFieldFinal f then acc_final else 0)
    writeConstRef $ Utf8ConstRef (jFieldName f)
    writeConstRef $ Utf8ConstRef (fieldDescriptorString $ jFieldType f)
    lift $ putWord16be 0  -- attributes_count

writeMethod :: String -> JMethod -> ConstantPoolGen ()
writeMethod className m = do
    lift $ putWord16be $ visibilityToAccessFlags (jMethodVisibility m) .|.
        (if jMethodStatic m then acc_static else 0) .|.
        finalityToAccessFlags (jMethodFinality m) .|.
        (if jMethodStrictfp m then acc_strict else 0)
    writeConstRef $ Utf8ConstRef (jMethodName m)
    writeConstRef $ Utf8ConstRef (methodDescriptorString (jMethodSignature m))
    lift $ putWord16be 1  -- attributes_count
    writeConstRef $ Utf8ConstRef "Code"
    -- attribute_length
    lift $ putWord32be (fromIntegral ((if writeStackMap then 20 else 12) +
        codeSize + stackMapTableSize))
    lift $ putWord16be (maxStack m)  -- max_stack
    lift $ putWord16be (maxLocals m)  -- max_locals
    lift $ putWord32be (fromIntegral codeSize)  -- code_length
    let writeInstrAndInc offset instr = offset `seq` do
            writeInstr predictedOffsets offset instr
            return $ offset + predictInstrSize offset instr
    foldM_ writeInstrAndInc 0 $ concat blockOrder
    lift $ putWord16be 0  -- exception_table_length
    lift $ putWord16be (if writeStackMap then 1 else 0)  -- attributes_count
    when writeStackMap $ do
        writeConstRef $ Utf8ConstRef "StackMapTable"
        lift $ putWord32be (fromIntegral (2 + stackMapTableSize))  -- attribute_length
        lift $ putWord16be (fromIntegral (length stackMapTable))  -- number_of_entries
        forM_ stackMapTable writeStackMapFrame
  where blockOrder = jMethodCode m
        blockOffsetsList = scanl (foldl' addInstrSize) 0 blockOrder
        addInstrSize thisOffset instr = thisOffset + predictInstrSize thisOffset instr
        predictedOffsets = Map.fromList $ zip [0..length blockOrder - 1] blockOffsetsList
        codeSize = last blockOffsetsList
        stackMapTable = buildStackMapTable className m blockOffsetsList
        writeStackMap = not (null stackMapTable)
        stackMapTableSize = sum (map predictStackMapFrameSize stackMapTable)

maxLocals :: JMethod -> Word16
maxLocals m = max signatureLocals instrLocals
  where signatureLocals = (if jMethodStatic m then 0 else 1) + paramLocals
        paramLocals = fromIntegral $ length $ jSignatureParamTypes $ jMethodSignature m
        instrLocals = foldl' max 0 $ map necessaryLocalLimit instrs
        instrs = concat (jMethodCode m)

necessaryLocalLimit :: JInstr -> Word16
necessaryLocalLimit (JInstr_aload  i) = i + 1
necessaryLocalLimit (JInstr_astore i) = i + 1
necessaryLocalLimit (JInstr_iload  i) = i + 1
necessaryLocalLimit _ = 0

maxStack :: JMethod -> Word16
maxStack m = maximum (Map.elems blockMaxStackSizes)
  where blockMaxStackSizes = visitBlocks m (Map.singleton 0 0) Map.empty (Set.singleton 0)

visitBlocks :: JMethod -> Map.Map JBlockLabel Word16 -> Map.Map JBlockLabel Word16 ->
    Set.Set JBlockLabel -> Map.Map JBlockLabel Word16
visitBlocks m !blockEntryStackSizes !blockMaxStackSizes toVisit  -- Strict accumulating parameters
    | Just (b, toVisitReduced) <- Set.minView toVisit =
        let bCode = jMethodCode m !! b
            bCodeStackSizes = scanl
                (\stackSize i -> fromIntegral (fromIntegral stackSize + predictStackUsage i))
                (blockEntryStackSizes Map.! b) bCode
            (blockDests, blockEntryStackSizes') = visitInstrs b blockEntryStackSizes $
                zip bCode (tail bCodeStackSizes)
            blockMaxStackSizes' = Map.insert b (maximum bCodeStackSizes) blockMaxStackSizes
            toVisit' = Set.union toVisitReduced $
                Set.filter (flip Map.notMember blockEntryStackSizes) blockDests
        in visitBlocks m blockEntryStackSizes' blockMaxStackSizes' toVisit'
    | otherwise = blockMaxStackSizes

visitInstrs :: JBlockLabel -> Map.Map JBlockLabel Word16 -> [(JInstr, Word16)] ->
    (Set.Set JBlockLabel, Map.Map JBlockLabel Word16)
visitInstrs thisBlock initialSizes = go Set.empty initialSizes
  where go _ _ [] = error "visitInstrs: empty list"
        go d s [iInfo] = result d s iInfo True
        go d s (iInfo:rest) =
            let (d', s') = result d s iInfo False
            in d' `seq` s' `seq` go d' s' rest
        result blockDests blockEntryStackSizes (i, stackSizeAfter) isLast =
            (blockDests', blockEntryStackSizes')
          where blockDests' = newDests `Set.union` blockDests
                blockEntryStackSizes' =
                    Map.unionWith requireIdentical newValues blockEntryStackSizes
                newValues = Map.fromList $ zip (Set.toList newDests) (repeat stackSizeAfter)
                newDests = branchFunc (possibleBranchDestinations i)
                branchFunc = if isLast then exitPoints thisBlock else filterToBranches

requireIdentical :: Eq a => a -> a -> a
requireIdentical x y
    | x == y = x
    | otherwise = error "Mismatched stack depths."

data BranchDest = Fallthrough | BranchTo !JBlockLabel
    deriving (Show, Eq, Ord)

filterToBranches :: Set.Set BranchDest -> Set.Set JBlockLabel
filterToBranches = Set.fromList . f . Set.toList
  where f [] = []
        f (Fallthrough : rest) = f rest
        f (BranchTo b : rest) = b : f rest

exitPoints :: JBlockLabel -> Set.Set BranchDest -> Set.Set JBlockLabel
exitPoints thisBlock = Set.map f
  where f Fallthrough = nextBlock
        f (BranchTo b) = b
        nextBlock = thisBlock + 1

possibleBranchDestinations :: JInstr -> Set.Set BranchDest
possibleBranchDestinations (JInstr_goto b) = Set.singleton (BranchTo b)
possibleBranchDestinations (JInstr_if _ b) = Set.fromList [Fallthrough, BranchTo b]
possibleBranchDestinations (JInstr_if_icmp _ b) = Set.fromList [Fallthrough, BranchTo b]
possibleBranchDestinations (JInstr_lookupswitch defaultBranch branches) =
    Set.insert (BranchTo defaultBranch) $ Set.fromList $ map BranchTo $ Map.elems branches
possibleBranchDestinations (JInstr_tableswitch defaultBranch _ branches) =
    Set.insert (BranchTo defaultBranch) $ Set.fromList $ map BranchTo branches
possibleBranchDestinations JInstr_return = Set.empty
possibleBranchDestinations _ = Set.singleton Fallthrough

predictStackUsage :: JInstr -> Int
predictStackUsage JInstr_aaload = -1
predictStackUsage (JInstr_aload _) = 1
predictStackUsage (JInstr_astore _) = -1
predictStackUsage (JInstr_checkcast _) = 0
predictStackUsage JInstr_dup = 1
predictStackUsage JInstr_dup_x1 = 1
predictStackUsage JInstr_dup_x2 = 1
predictStackUsage (JInstr_getstatic _ _ _) = 1
predictStackUsage (JInstr_goto _) = 0
predictStackUsage JInstr_iadd = -1
predictStackUsage JInstr_iastore = -3
predictStackUsage JInstr_idiv = -1
predictStackUsage (JInstr_if _ _) = -1
predictStackUsage (JInstr_if_icmp _ _) = -2
predictStackUsage (JInstr_iload _) = 1
predictStackUsage JInstr_imul = -1
predictStackUsage (JInstr_invoke invokeType _ _ (JSignature paramTypes returnType)) =
    let invokeTypeValue = case invokeType of { Static -> 0; _ -> -1 }
        paramTypesValue = -length paramTypes
        returnTypeValue = case returnType of { Nothing -> 0; _ -> 1 }
    in invokeTypeValue + paramTypesValue + returnTypeValue
predictStackUsage JInstr_irem = -1
predictStackUsage JInstr_isub = -1
predictStackUsage (JInstr_ldc _) = 1
predictStackUsage (JInstr_lookupswitch _ _) = -1
predictStackUsage (JInstr_new _) = 1
predictStackUsage (JInstr_newarray _) = 0
predictStackUsage JInstr_pop = -1
predictStackUsage (JInstr_putstatic _ _ _) = -1
predictStackUsage JInstr_return = 0
predictStackUsage JInstr_swap = 0
predictStackUsage (JInstr_tableswitch _ _ _) = -1

predictInstrSize :: Int -> JInstr -> Int
predictInstrSize _ JInstr_aaload = 1
predictInstrSize _ (JInstr_aload n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
predictInstrSize _ (JInstr_astore n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
predictInstrSize _ (JInstr_checkcast _) = 3
predictInstrSize _ JInstr_dup = 1
predictInstrSize _ JInstr_dup_x1 = 1
predictInstrSize _ JInstr_dup_x2 = 1
predictInstrSize _ (JInstr_getstatic _ _ _) = 3
predictInstrSize _ (JInstr_goto _) = 3
predictInstrSize _ JInstr_iadd = 1
predictInstrSize _ JInstr_iastore = 1
predictInstrSize _ JInstr_idiv = 1
predictInstrSize _ (JInstr_if _ _) = 3
predictInstrSize _ (JInstr_if_icmp _ _) = 3
predictInstrSize _ (JInstr_iload n)
    | n <= 3 = 1
    | n <= 255 = 2
    | otherwise = 4
predictInstrSize _ JInstr_imul = 1
predictInstrSize _ (JInstr_invoke _ _ _ _) = 3
predictInstrSize _ JInstr_irem = 1
predictInstrSize _ JInstr_isub = 1
predictInstrSize _ (JInstr_ldc (IntValue i))
    | i >= -1 && i <= 5 = 1
    | i >= -128 && i <= 127 = 2
predictInstrSize _ (JInstr_ldc _) = 3
predictInstrSize thisOffset (JInstr_lookupswitch _ branches) =
    1 + (3 - thisOffset `rem` 4) + 8 + 8 * Map.size branches
predictInstrSize _ (JInstr_new _) = 3
predictInstrSize _ (JInstr_newarray _) = 2
predictInstrSize _ JInstr_pop = 1
predictInstrSize _ (JInstr_putstatic _ _ _) = 3
predictInstrSize _ JInstr_return = 1
predictInstrSize _ JInstr_swap = 1
predictInstrSize thisOffset (JInstr_tableswitch _ _ branches) =
    1 + (3 - thisOffset `rem` 4) + 12 + 4 * length branches

writeInstr :: Map.Map JBlockLabel Int -> Int -> JInstr -> ConstantPoolGen ()
writeInstr _ _ JInstr_aaload = lift $ putWord8 0x32
writeInstr _ _ (JInstr_aload n)
    | n <= 3 = lift $ putWord8 (fromIntegral (0x2a + n))
    | n <= 255 = do
        lift $ putWord8 0x19
        lift $ putWord8 (fromIntegral n)
    | otherwise = do
        lift $ putWord8 0xc4
        lift $ putWord8 0x19
        lift $ putWord16be n
writeInstr _ _ (JInstr_astore n)
    | n <= 3 = lift $ putWord8 (fromIntegral (0x4b + n))
    | n <= 255 = do
        lift $ putWord8 0x3a
        lift $ putWord8 (fromIntegral n)
    | otherwise = do
        lift $ putWord8 0xc4
        lift $ putWord8 0x3a
        lift $ putWord16be n
writeInstr _ _ (JInstr_checkcast className) = do
    lift $ putWord8 0xc0
    writeConstRef $ ClassConstRef className
writeInstr _ _ JInstr_dup = lift $ putWord8 0x59
writeInstr _ _ JInstr_dup_x1 = lift $ putWord8 0x5a
writeInstr _ _ JInstr_dup_x2 = lift $ putWord8 0x5b
writeInstr _ _ (JInstr_getstatic className fieldName fieldType) = do
    lift $ putWord8 0xb2
    writeConstRef $ FieldRefConstRef className fieldName fieldType
writeInstr labelOffsets thisOffset (JInstr_goto b) = do
    lift $ putWord8 0xa7
    lift $ putWord16be $ fromIntegral $ (labelOffsets Map.! b) - thisOffset
writeInstr _ _ JInstr_iadd = lift $ putWord8 0x60
writeInstr _ _ JInstr_iastore = lift $ putWord8 0x4f
writeInstr _ _ JInstr_idiv = lift $ putWord8 0x6c
writeInstr labelOffsets thisOffset (JInstr_if cmp b) = do
    lift $ putWord8 $ 0x99 + fromIntegral (fromEnum cmp)
    lift $ putWord16be $ fromIntegral $ (labelOffsets Map.! b) - thisOffset
writeInstr labelOffsets thisOffset (JInstr_if_icmp cmp b) = do
    lift $ putWord8 $ 0x9f + fromIntegral (fromEnum cmp)
    lift $ putWord16be $ fromIntegral $ (labelOffsets Map.! b) - thisOffset
writeInstr _ _ (JInstr_iload n)
    | n <= 3 = lift $ putWord8 (fromIntegral (0x1a + n))
    | n <= 255 = do
        lift $ putWord8 0x15
        lift $ putWord8 (fromIntegral n)
    | otherwise = do
        lift $ putWord8 0xc4
        lift $ putWord8 0x15
        lift $ putWord16be n
writeInstr _ _ JInstr_imul = lift $ putWord8 0x68
writeInstr _ _ (JInstr_invoke invokeType className methodName signature) = do
    lift $ putWord8 (invokeCode invokeType)
    writeConstRef $ MethodRefConstRef className methodName signature
writeInstr _ _ JInstr_irem = lift $ putWord8 0x70
writeInstr _ _ JInstr_isub = lift $ putWord8 0x64
writeInstr _ _ (JInstr_ldc (IntValue i))
    | i >= -1 && i <= 5 = lift $ putWord8 (fromIntegral (0x03 + i))
    | i >= -128 && i <= 127 = do
        lift $ putWord8 0x10
        lift $ putWord8 (fromIntegral i)
    | i >= -32768 && i < 32767 = do
        lift $ putWord8 0x17
        lift $ putWord16be (fromIntegral i)
    | otherwise = do
        lift $ putWord8 0x13
        writeConstRef $ IntConstRef i
writeInstr _ _ (JInstr_ldc (MethodHandle_invokestatic_Value className methodName signature)) = do
    lift $ putWord8 0x13
    writeConstRef $ MethodHandleInvokeStaticConstRef className methodName signature
writeInstr _ _ (JInstr_ldc (StringValue s)) = do
    lift $ putWord8 0x13
    writeConstRef $ StringConstRef s
writeInstr labelOffsets thisOffset (JInstr_lookupswitch defaultBranch branches) = do
    lift $ putWord8 0xab
    padSwitchInstr thisOffset
    lift $ putWord32be $ fromIntegral $ (labelOffsets Map.! defaultBranch) - thisOffset
    lift $ putWord32be $ fromIntegral (Map.size branches)
    forM_ (Map.toList branches) $ \(k, b) -> do
        lift $ putWord32be $ fromIntegral k
        lift $ putWord32be $ fromIntegral $ (labelOffsets Map.! b) - thisOffset
writeInstr _ _ (JInstr_new className) = do
    lift $ putWord8 0xbb
    writeConstRef $ ClassConstRef className
writeInstr _ _ (JInstr_newarray t) = do
    lift $ putWord8 0xbc
    lift $ putWord8 (newarrayTypeCode t)
writeInstr _ _ JInstr_pop = lift $ putWord8 0x57
writeInstr _ _ (JInstr_putstatic className fieldName fieldType) = do
    lift $ putWord8 0xb3
    writeConstRef $ FieldRefConstRef className fieldName fieldType
writeInstr _ _ JInstr_return = lift $ putWord8 0xb1
writeInstr _ _ JInstr_swap = lift $ putWord8 0x5f
writeInstr labelOffsets thisOffset (JInstr_tableswitch defaultBranch lowNum branches) = do
    lift $ putWord8 0xaa
    padSwitchInstr thisOffset
    lift $ putWord32be $ fromIntegral $ (labelOffsets Map.! defaultBranch) - thisOffset
    lift $ putWord32be $ fromIntegral lowNum
    lift $ putWord32be $ fromIntegral lowNum + fromIntegral (length branches) - 1
    forM_ branches $ \b -> lift $ putWord32be $ fromIntegral $ (labelOffsets Map.! b) - thisOffset

invokeCode :: JInvokeType -> Word8
invokeCode Special = 0xb7
invokeCode Static  = 0xb8
invokeCode Virtual = 0xb6

newarrayTypeCode :: JPrimitiveType -> Word8
newarrayTypeCode JBoolean = 4
newarrayTypeCode JChar    = 5
newarrayTypeCode JFloat   = 6
newarrayTypeCode JDouble  = 7
newarrayTypeCode JByte    = 8
newarrayTypeCode JShort   = 9
newarrayTypeCode JInt     = 10
newarrayTypeCode JLong    = 11

padSwitchInstr :: Int -> ConstantPoolGen ()
padSwitchInstr thisOffset = forM_ [0..2 - (thisOffset `rem` 4)] $ const $ lift $ putWord8 0x00

initialStackMapFrame :: String -> JMethod -> JStackMapFrame
initialStackMapFrame className m = JStackMapFrame locals []
  where locals = thisLocal ++ paramLocals
        thisLocal = if jMethodStatic m then [] else [VT_object className]
        paramLocals = map jTypeToVerificationType $ jSignatureParamTypes (jMethodSignature m)

jTypeToVerificationType :: JType -> JVerificationType
jTypeToVerificationType (JPrimitiveType JFloat) = VT_float
jTypeToVerificationType (JPrimitiveType JDouble) = VT_double
jTypeToVerificationType (JPrimitiveType JLong) = VT_long
jTypeToVerificationType (JPrimitiveType _) = VT_int
jTypeToVerificationType (JObjectType className) = VT_object className
jTypeToVerificationType (JArrayType _) = error "Array types not handled yet..."

predictVerificationTypeSize :: JVerificationType -> Int
predictVerificationTypeSize (VT_uninitialized _) = 3
predictVerificationTypeSize (VT_object _) = 3
predictVerificationTypeSize _ = 1

writeVerificationType :: JVerificationType -> ConstantPoolGen ()
writeVerificationType VT_top = lift $ putWord8 0
writeVerificationType VT_null = lift $ putWord8 5
writeVerificationType (VT_uninitialized offset) = do
    lift $ putWord8 8
    lift $ putWord16be offset
writeVerificationType VT_uninitializedThis = lift $ putWord8 6
writeVerificationType VT_int = lift $ putWord8 1
writeVerificationType VT_long = lift $ putWord8 4
writeVerificationType VT_float = lift $ putWord8 2
writeVerificationType VT_double = lift $ putWord8 3
writeVerificationType (VT_object className) = do
    lift $ putWord8 7
    writeConstRef $ ClassConstRef className

data CompressedFrame = CompressedFrame !Word16 !CompressedFrameType
    deriving (Show)
data CompressedFrameType
    = SameFrame
    | SameLocals1StackItemFrame JVerificationType
    | ChopFrame Word8
    | AppendFrame [JVerificationType]
    | FullFrame [JVerificationType] [JVerificationType]
  deriving (Show)

predictStackMapFrameSize :: CompressedFrame -> Int
predictStackMapFrameSize (CompressedFrame offsetDelta SameFrame)
    | offsetDelta < 64 = 1
    | otherwise = 3
predictStackMapFrameSize (CompressedFrame offsetDelta (SameLocals1StackItemFrame stackItem))
    | offsetDelta < 64 = 1 + predictVerificationTypeSize stackItem
    | otherwise = 3 + predictVerificationTypeSize stackItem
predictStackMapFrameSize (CompressedFrame _ (ChopFrame _)) = 3
predictStackMapFrameSize (CompressedFrame _ (AppendFrame locals)) =
    3 + sum (map predictVerificationTypeSize locals)
predictStackMapFrameSize (CompressedFrame _ (FullFrame locals stack)) =
    7 + sum (map predictVerificationTypeSize locals)
      + sum (map predictVerificationTypeSize stack)

writeStackMapFrame :: CompressedFrame -> ConstantPoolGen ()
writeStackMapFrame (CompressedFrame offsetDelta SameFrame)
    | offsetDelta < 64 = lift $ putWord8 (fromIntegral offsetDelta)
    | otherwise = do
        lift $ putWord8 251
        lift $ putWord16be offsetDelta
writeStackMapFrame (CompressedFrame offsetDelta (SameLocals1StackItemFrame stackItem))
    | offsetDelta < 64 = do
        lift $ putWord8 (64 + fromIntegral offsetDelta)
        writeVerificationType stackItem
    | otherwise = do
        lift $ putWord8 247
        lift $ putWord16be offsetDelta
        writeVerificationType stackItem
writeStackMapFrame (CompressedFrame offsetDelta (ChopFrame k)) = do
    lift $ putWord8 (251 - k)
    lift $ putWord16be offsetDelta
writeStackMapFrame (CompressedFrame offsetDelta (AppendFrame locals)) = do
    lift $ putWord8 (251 + fromIntegral (length locals))
    lift $ putWord16be offsetDelta
    forM_ locals writeVerificationType
writeStackMapFrame (CompressedFrame offsetDelta (FullFrame locals stack)) = do
    lift $ putWord8 255
    lift $ putWord16be offsetDelta
    lift $ putWord16be (fromIntegral (length locals))
    forM_ locals writeVerificationType
    lift $ putWord16be (fromIntegral (length stack))
    forM_ stack writeVerificationType

buildStackMapTable :: String -> JMethod -> [Int] -> [CompressedFrame]
buildStackMapTable className m blockOffsetsList =
    compressStackMapTable (-1) initialFrame filteredFrames
  where initialFrame = initialStackMapFrame className m
        -- We don't need to write out a frame for a block unless some instruction branches to it.
        filteredFrames = [(blockOffset, frame) |
            (i, (frame, _), blockOffset) <- zip3 [0..] (jMethodCodeBlocks m) blockOffsetsList,
            i `Set.member` allBranchDests]
        allBranchDests = filterToBranches $ Set.unions $
            map possibleBranchDestinations $ concat (jMethodCode m)

compressStackMapTable :: Int -> JStackMapFrame -> [(Int, JStackMapFrame)] -> [CompressedFrame]
compressStackMapTable _ _ [] = []
compressStackMapTable prevOffset prevFrame ((thisOffset, thisFrame) : restFrames) =
    compressedFrame : compressStackMapTable thisOffset thisFrame restFrames
  where compressedFrame = CompressedFrame offsetDelta (compressFrame prevFrame thisFrame)
        offsetDelta = fromIntegral (thisOffset - prevOffset) - 1

compressFrame :: JStackMapFrame -> JStackMapFrame -> CompressedFrameType
compressFrame (JStackMapFrame prevLocals _) (JStackMapFrame thisLocals thisStack) =
    case thisStack of
        []  | thisLocals == prevLocals -> SameFrame
            | Just removedLocals <- stripPrefix thisLocals prevLocals,
                let k = length removedLocals, k <= 3 -> ChopFrame (fromIntegral k)
            | Just addedLocals <- stripPrefix prevLocals thisLocals,
                let k = length addedLocals, k <= 3 -> AppendFrame addedLocals
        [stackItem] | thisLocals == prevLocals -> SameLocals1StackItemFrame stackItem
        _ -> FullFrame thisLocals thisStack

data ConstRef
    = ClassConstRef String
    | FieldRefConstRef String String JType
    | MethodRefConstRef String String JSignature
    | StringConstRef String
    | IntConstRef !Int32
    | NameAndTypeConstRef String String
    | Utf8ConstRef String
    | MethodHandleInvokeStaticConstRef String String JSignature
  deriving (Show, Eq, Ord)

type ConstantPoolGen = StateT (Word16, Map.Map ConstRef Word16) PutM

runConstantPoolGen :: ConstantPoolGen a -> ((Word16, Map.Map ConstRef Word16), B.ByteString)
runConstantPoolGen = runPutM . flip execStateT (1, Map.empty)

getConstRefIndex :: ConstRef -> ConstantPoolGen Word16
getConstRefIndex ref = do
    (nextIndex, constRefs) <- get
    case ref `Map.lookup` constRefs of
        Nothing -> do
            put (nextIndex + constRefEntryCount ref, Map.insert ref nextIndex constRefs)
            addDependenciesToPool ref
            return nextIndex
        Just index -> return index
addDependenciesToPool :: ConstRef -> ConstantPoolGen ()
addDependenciesToPool (ClassConstRef className) = do
    getConstRefIndex $ Utf8ConstRef (modifiedClassName className)
    return ()
addDependenciesToPool (NameAndTypeConstRef name descriptor) = do
    getConstRefIndex $ Utf8ConstRef name
    getConstRefIndex $ Utf8ConstRef descriptor
    return ()
addDependenciesToPool (FieldRefConstRef className fieldName fieldType) = do
    getConstRefIndex $ ClassConstRef className
    getConstRefIndex $ NameAndTypeConstRef fieldName (fieldDescriptorString fieldType)
    return ()
addDependenciesToPool (MethodRefConstRef className methodName signature) = do
    getConstRefIndex $ ClassConstRef className
    getConstRefIndex $ NameAndTypeConstRef methodName (methodDescriptorString signature)
    return ()
addDependenciesToPool (StringConstRef s) = do
    getConstRefIndex $ Utf8ConstRef s
    return ()
addDependenciesToPool (MethodHandleInvokeStaticConstRef className methodName signature) = do
    getConstRefIndex $ MethodRefConstRef className methodName signature
    return ()
addDependenciesToPool _ = return ()

writeConstRef :: ConstRef -> ConstantPoolGen ()
writeConstRef ref = do
    index <- getConstRefIndex ref
    lift $ putWord16be index

constRefEntryCount :: ConstRef -> Word16
constRefEntryCount _ = 1

writeConstantPool :: Word16 -> Map.Map ConstRef Word16 -> PutM ()
writeConstantPool constPoolCount constRefs = do
    putWord16be constPoolCount
    let sortedConstRefs = sortBy (compare `on` (constRefs Map.!)) $ Map.keys constRefs
    forM_ sortedConstRefs $ \ref ->
        case ref of
            ClassConstRef className -> do
                putWord8 7
                putWord16be (constRefs Map.! Utf8ConstRef (modifiedClassName className))
            FieldRefConstRef className fieldName fieldType -> do
                putWord8 9
                putWord16be (constRefs Map.! ClassConstRef className)
                putWord16be (constRefs Map.! NameAndTypeConstRef fieldName (fieldDescriptorString fieldType))
            MethodRefConstRef className methodName signature -> do
                putWord8 10
                putWord16be (constRefs Map.! ClassConstRef className)
                putWord16be (constRefs Map.! NameAndTypeConstRef methodName (methodDescriptorString signature))
            StringConstRef s -> do
                putWord8 8
                putWord16be (constRefs Map.! Utf8ConstRef s)
            IntConstRef value -> do
                putWord8 3
                putWord32be (fromIntegral value)
            NameAndTypeConstRef name descriptor -> do
                putWord8 12
                putWord16be (constRefs Map.! Utf8ConstRef name)
                putWord16be (constRefs Map.! Utf8ConstRef descriptor)
            Utf8ConstRef s -> do
                putWord8 1
                putWord16be $ fromIntegral (length s)
                -- TODO: Implement behavior for non-ASCII characters.
                forM_ s $ putWord8 . fromIntegral . fromEnum
            MethodHandleInvokeStaticConstRef className methodName signature -> do
                putWord8 15
                putWord8 6
                putWord16be (constRefs Map.! MethodRefConstRef className methodName signature)

modifiedClassName :: String -> String
modifiedClassName = map replaceDot
  where replaceDot '.' = '/'
        replaceDot c   = c

fileNameFromClassName :: String -> FilePath
fileNameFromClassName = (++ ".class") . modifiedClassName

visibilityToAccessFlags :: JVisibility -> Word16
visibilityToAccessFlags JPrivate        = acc_private
visibilityToAccessFlags JPackagePrivate = 0
visibilityToAccessFlags JProtected      = acc_protected
visibilityToAccessFlags JPublic         = acc_public

finalityToAccessFlags :: JFinality -> Word16
finalityToAccessFlags JOverridable = 0
finalityToAccessFlags JAbstract    = acc_abstract
finalityToAccessFlags JFinal       = acc_final

fieldDescriptorString :: JType -> String
fieldDescriptorString (JPrimitiveType JByte)    = "B"
fieldDescriptorString (JPrimitiveType JChar)    = "C"
fieldDescriptorString (JPrimitiveType JDouble)  = "D"
fieldDescriptorString (JPrimitiveType JFloat)   = "F"
fieldDescriptorString (JPrimitiveType JInt)     = "I"
fieldDescriptorString (JPrimitiveType JLong)    = "J"
fieldDescriptorString (JPrimitiveType JShort)   = "S"
fieldDescriptorString (JPrimitiveType JBoolean) = "Z"
fieldDescriptorString (JObjectType className) = "L" ++ modifiedClassName className ++ ";"
fieldDescriptorString (JArrayType elemType) = '[' : fieldDescriptorString elemType

methodDescriptorString :: JSignature -> String
methodDescriptorString JSignature { jSignatureParamTypes = paramTypes, jSignatureReturnType = returnType } =
    let returnDescriptor =
            case returnType of
                Nothing -> "V"
                Just t -> fieldDescriptorString t
    in "(" ++ concat (map fieldDescriptorString paramTypes) ++ ")" ++ returnDescriptor

acc_public, acc_private, acc_protected, acc_static, acc_final, acc_super,
    acc_abstract, acc_strict :: Word16
acc_public    = 0x0001
acc_private   = 0x0002
acc_protected = 0x0004
acc_static    = 0x0008
acc_final     = 0x0010
acc_super     = 0x0020
acc_abstract  = 0x0400
acc_strict    = 0x0800
