import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.List (delete)
import Data.Maybe (isJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Verbosity

checkLLVM :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
checkLLVM (pkg, hbi) inflags = do
  installed <- isJust <$> findProgramLocation verbose "llvm-config"
  let llvmf = FlagName "llvm"
      cfgflags = configConfigurationsFlags inflags
      fixcfgflags = (llvmf, False) : delete (llvmf, True) cfgflags
      fixflags = inflags { configConfigurationsFlags = fixcfgflags }
  let (flags, msg) = case installed of
        True -> (inflags, False)
        False -> case lookup llvmf cfgflags of
          Just True -> (fixflags, True)
          Just False -> (inflags, False)
          Nothing -> (fixflags, True)
  when msg $ do
    putStrLn "Warning: LLVM is not installed."
    putStrLn "Building arb-fft with native code generator (slower)."
  configure (pkg, hbi) flags

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = checkLLVM }
