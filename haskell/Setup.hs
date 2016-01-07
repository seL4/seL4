module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.Process
import System.Exit
import Control.Monad
import Data.List
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    let targetPrefix = "--configure-option="
    let targetArg = find (targetPrefix `isPrefixOf`) args
    let targetName = liftM (drop (length targetPrefix)) targetArg
    hooks <- return $ autoconfUserHooks {
        readDesc = readDescHook targetName
    }
    defaultMainWithHooksArgs hooks args


buildLibQEmu :: Args -> BuildFlags -> IO HookedBuildInfo
buildLibQEmu args flags = do
    putStrLn "Building haskell sel4 ..."
    info <- (preBuild autoconfUserHooks) args flags
    return info

printKnownTargets :: IO ()
printKnownTargets = do
    putStrLn "Recognised targets are:"
    mapM_ (putStrLn.('\t':).fst) targets

targets =
    [ ("arm-exynos",     ("ARM", "Exynos4210"))
    , ("arm-kzm", ("ARM", "KZM"))
    , ("arm-sabre", ("ARM", "SABRE"))
    , ("x64-pc99", ("X64", "PC99"))
    ]

getPlatform targetName = do
      return $ do
        targetName <- targetName
        (name, target) <- find ((==targetName).fst) targets
        return $ target

readDescHook :: Maybe String -> IO (Maybe GenericPackageDescription)
readDescHook targetName = do
    platform <- getPlatform targetName
    (arch, plat) <- case platform of
       Just p -> return p
       Nothing -> do
         putStrLn "Please specify a target: --configure-option=\"<target>\""
         printKnownTargets
         fail "No target"
    dscp <- readPackageDescription normal $ "SEL4.cabal"
    pkg_lib <- return $ condLibrary dscp
    pkg_lib_upd <- case pkg_lib of
      Just (CondNode {condTreeData = lib,condTreeConstraints = cons,condTreeComponents=comp}) -> do
        bi_upd <- do
          bi <- return $ libBuildInfo lib
          opts <- return $ (cppOptions bi) ++ ["-DPLATFORM=" ++ plat] ++ ["-DTARGET=" ++ arch]
                                           ++ ["-DPLATFORM_" ++ plat] ++ ["-DTARGET_" ++ arch]
          return $ bi { cppOptions = opts }
        return $ Just $ (CondNode {condTreeData = (lib { libBuildInfo = bi_upd}),
                 condTreeConstraints = cons, condTreeComponents = comp})
      Nothing -> return Nothing
    dscp_upd <- return $ dscp { condLibrary = pkg_lib_upd}
    print dscp_upd
    return $ Just dscp_upd
