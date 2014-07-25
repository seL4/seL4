#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

-- This is to shut GHC up about defaultUserHooks.

module Main where
import Control.Applicative((<$>))
import Distribution.Simple
import Distribution.PackageDescription
import System.Directory
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import qualified Distribution.ModuleName
import Distribution.Simple.Setup
import System.Cmd
import System.Exit
import System.Environment(getArgs)
import Data.List(isPrefixOf, find)
import Control.Monad(unless, liftM)

targets =
    [ ("arm-qemu",     ("ARM", "QEmu"))
    , ("arm-lyrebird", ("ARM", "Lyrebird"))]

bootModules =
    [ "Kernel/CSpace"
    , "Kernel/Thread"
    , "Kernel/FaultHandler"
    , "Kernel/VSpace"
    , "Kernel/Init"
    , "Model/PSpace"
    , "Object/TCB"
    , "Object/CNode"
    , "Object/ObjectType"
    ]

platformModules =
    [ "Machine.Hardware"
    ]

archModules =
    [ "API.Types"
    , "API.Invocation"
    , "Kernel.VSpace"
    , "Kernel.Thread"
    , "Object.ObjectType"
    , "Object.Structures"
    , "Object.Interrupt"
    , "Object.Instances"
    , "Object.TCB"
    , "Model.StateData"
    , "Machine.RegisterSet"
    , "Machine.Hardware"
    ]

targetModules arch platform =
    (map (\a -> "SEL4." ++ a ++ "." ++ arch) archModules) ++
    (map (\a -> "SEL4." ++ a ++ "." ++ arch_platform) platformModules)
    where arch_platform = arch ++ "." ++ platform

ghcOptions arch platform =
    [ "-DTARGET=" ++ arch
    , "-DTARGET_" ++ arch
    , "-DPLATFORM=" ++ platform
    , "-DPLATFORM_" ++ platform
    ]

bootFiles = map moduleToLHSName bootModules
    where
        moduleToLHSName f = "src/SEL4/" ++
            (map (\a -> if a == '.' then '/' else a) f) ++ ".lhs"

main :: IO ()
main = do
    args <- getArgs
    let targetPrefix = "--with-target="
    let targetArg = find (targetPrefix `isPrefixOf`) args
    let targetName = liftM (drop (length targetPrefix)) targetArg
    let args' = filter (not . isPrefixOf targetPrefix) args
              -- n.b.  GHC 7.0.3 whinges about defaultUserHooks being
              -- deprecated, and demands simpleUserHooks or autoconfUserHooks
              -- instead.  Neither of those actually works here, so I'm stuck
              -- with defaultUserHooks.
    let hooks = simpleUserHooks {
                    preBuild = \args flags -> do
                        generateHSBoot
                        (preBuild simpleUserHooks) args flags,
                    readDesc = do
                      cabalfile <- generateTempCabalFile "SEL4.cabal" targetName
                      v <- readPackageDescription normal $ cabalfile
                      removeFile cabalfile
                      return $ Just v
                    }
    defaultMainWithHooksArgs hooks args'

printKnownTargets :: IO ()
printKnownTargets = do
    putStrLn "Recognised targets are:"
    mapM_ (putStrLn.('\t':).fst) targets


generateTempCabalFile :: String -> Maybe String -> IO String
generateTempCabalFile fn targetName = do
    case (do name <- targetName
             (name, (arch, platform)) <- find ((==name).fst) targets
             return (name, arch, platform)) of
      Nothing -> do
            putStrLn "Please specify a target: --with-target=<target>"
            printKnownTargets
            fail "No target"
      Just (name, arch, platform) -> do
         lns <- fmap lines $ readFile fn
         writeFile (fn ++ "." ++ name) $ unlines (concatMap (f arch platform) lns)
         return (fn ++ "." ++ name)
 where f arch platform (span (== ' ') -> (length -> indentation, str)) | "$TARGET-SPECIFIC-STUFF" `isPrefixOf` str = generateArchSpecificLines arch platform indentation
       f _ _ x = [x]

generateArchSpecificLines :: String -> String -> Int -> [String]
generateArchSpecificLines arch platform indentation =
    [replicate indentation ' ' ++ "other-modules:" ++ unwords (targetModules arch platform)
    ,replicate indentation ' ' ++ "cpp-options:" ++ unwords (ghcOptions arch platform)]

generateHSBoot :: IO ()
generateHSBoot = mapM_ generateHSBoot' bootFiles
        where
                generateHSBoot' n = do
                        putStrLn ("Generating boot file for " ++ n)
                        r <- system ("perl mkhsboot.pl -l <" ++ n ++ " >"
                                          ++ n ++ "-boot")
                        unless (r == ExitSuccess) $ error "Couldn't generate boot file"


