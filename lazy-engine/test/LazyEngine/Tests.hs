import Test.Framework
import Test.Framework.Providers.HUnit(hUnitTestToTests)

import qualified LazyEngine.OperationalToGMachineTests

main :: IO ()
main = defaultMain allTests

allTests :: [Test]
allTests = [
    testGroup "OperationalToGMachineTests" $
        hUnitTestToTests LazyEngine.OperationalToGMachineTests.tests
  ]
