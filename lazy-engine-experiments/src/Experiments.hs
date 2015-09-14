import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
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
    mapM_ (uncurry writeClassFile) classFiles
  where gModule = operationalToGMachine operationalModule
        javaClasses = gMachineToJavaClassFile generatedClassName gModule
        classFiles = [(jClassName javaClass, javaClassFileToBinary javaClass) |
            javaClass <- javaClasses]

writeClassFile :: String -> B.ByteString -> IO ()
writeClassFile className classFile = do
    B.writeFile classFileName classFile
    withFile disassemblyFileName WriteMode $ \disassemblyFileHandle -> do
        let processArgs = (P.proc "javap"
                ["-v", "-l", "-p", "-c", "-s", "-constants", className])
                { P.std_out = P.UseHandle disassemblyFileHandle }
        (_, _, _, javapProcess) <- P.createProcess processArgs
        void $ P.waitForProcess javapProcess
  where classFileName = fileNameFromClassName className
        disassemblyFileName = classFileName ++ ".txt"

main :: IO ()
main = writeOperationalModule testClassName testModule

testClassName :: String
testClassName = "example.ExampleGeneratedModule"

testModule :: O.Module
testModule = O.Module Map.empty (Map.fromList [
    (GlobalName "minusInt", O.Supercombinator [Just (LocalID 1), Just (LocalID 2)] $
        O.TermExpr $ O.global "minusInt" `O.Ap` O.local 1 `O.Ap` O.local 2),
    (GlobalName "main", O.Supercombinator [] $
        O.TermExpr $ O.global "factorial" `O.Ap` O.IntLiteral 6),
    (GlobalName "factorial", O.Supercombinator [Just (LocalID 1)] $
        O.Case (O.local 1) (LocalID 2) (Map.fromList [(O.IntPat 0, O.TermExpr $ O.IntLiteral 1)])
            (O.TermExpr $ O.global "timesInt" `O.Ap` O.local 2 `O.Ap` (O.global "factorial" `O.Ap`
                (O.global "minusInt" `O.Ap` O.local 2 `O.Ap` O.IntLiteral 1))))
  ])
