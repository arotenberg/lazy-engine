import Control.Monad
import qualified Data.ByteString.Lazy as B
import System.IO(withFile, IOMode(..))
import qualified System.Process as P

import LazyEngine.JVM.GMachineToJavaClassFile(gMachineToJavaClassFile)
import LazyEngine.JVM.JavaClassFile(jClassName)
import LazyEngine.JVM.JavaClassFileToBinary(fileNameFromClassName, javaClassFileToBinary)
import LazyEngine.Name
import qualified LazyEngine.Operational as O
import LazyEngine.OperationalToGMachine(operationalToGMachine)

writeOperationalModule :: String -> O.Module -> IO ()
writeOperationalModule generatedClassName operationalModule =
    case operationalToGMachine operationalModule of
        Left err -> putStrLn err
        Right gModule ->
            let javaClasses = gMachineToJavaClassFile generatedClassName gModule
                classFiles = [(jClassName javaClass, javaClassFileToBinary javaClass) |
                    javaClass <- javaClasses]
            in mapM_ (uncurry writeClassFile) classFiles 

writeClassFile :: String -> B.ByteString -> IO ()
writeClassFile className classFile = do
    let classFileName = fileNameFromClassName className
    B.writeFile classFileName classFile
    let disassemblyFileName = classFileName ++ ".txt"
    withFile disassemblyFileName WriteMode $ \disassemblyFileHandle -> do
        let processArgs = (P.proc "javap"
                ["-v", "-l", "-p", "-c", "-s", "-constants", className])
                { P.std_out = P.UseHandle disassemblyFileHandle }
        (_, _, _, javapProcess) <- P.createProcess processArgs
        void $ P.waitForProcess javapProcess

main :: IO ()
main = writeOperationalModule testClassName testModule

testClassName :: String
testClassName = "example.ExampleGeneratedModule"

testModule :: O.Module
testModule = O.Module [
    O.GlobalDecl (GlobalName "minusInt") [LocalID 1, LocalID 2] $
        O.EvalBinaryOp MinusOp (O.local 1) (O.local 2) (LocalID 3) $
        O.Return (O.local 3),
    O.GlobalDecl (GlobalName "main") [] $
        O.Let (LocalID 1) (O.ValueTerm (O.global "factorial") `O.Ap` O.IntLiteral 6) $
        O.Return (O.local 1),
    O.GlobalDecl (GlobalName "factorial") [LocalID 1] $
        O.Case (O.local 1) (LocalID 2)
          [(O.IntPat 0,
            O.Let (LocalID 3) (O.IntLiteral 1) $
            O.Return (O.local 3)
          )]
          (
            O.Let (LocalID 4) (O.ValueTerm (O.global "factorial") `O.Ap`
                    (O.ValueTerm (O.global "minusInt")
                        `O.Ap` O.ValueTerm (O.local 2)
                        `O.Ap` O.IntLiteral 1)) $
            O.EvalBinaryOp TimesOp (O.local 2) (O.local 4) (LocalID 5) $
            O.Return (O.local 5)
          )
  ]
