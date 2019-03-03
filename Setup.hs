import Control.Monad (when)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

checkLLVM :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
checkLLVM (pkg, hbi) flgs = do
  let warn = case lookupFlagAssignment (mkFlagName "llvm") $ configConfigurationsFlags flgs of
        Nothing    -> True
        Just True  -> False
        Just False -> True
  when warn $ do
    putStrLn "============================================================"
    putStrLn " WARNING: LLVM flag is not set."
    putStrLn ""
    putStrLn " Building arb-fft with native code generator (slower)."
    putStrLn " Reconfigure with -fllvm to build with LLVM."
    putStrLn "============================================================"
  configure (pkg, hbi) flgs

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = checkLLVM }
