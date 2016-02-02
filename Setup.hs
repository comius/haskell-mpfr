module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Ar
import Distribution.Simple.Program.Find
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription hiding (Flag)
import Distribution.Verbosity

import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Exit

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid
import Data.Maybe

-----------------------------------------------------------------
-- Horrible mess of semi-general code
--

-- Not particularly thread safe, but the whole notion of a current directory isn't either
inDirectory :: FilePath -> IO r -> IO r
inDirectory dir action = do
  old <- getCurrentDirectory
  setCurrentDirectory dir
  res <- action
  setCurrentDirectory old
  return res

mpfrRoot = "deps/mpfr"
getMpfrDist = do
   canonicalizePath "./deps/mpfrBuild"

pathsWithSuffix :: String -> FilePath -> IO [FilePath]
pathsWithSuffix suffix path = do
  files <- getDirectoryContents path
  return $ map (path </>) (filter (suffix `isSuffixOf`) files)


-- TODO: support Windows nicely
runOrBomb :: FilePath -> [String] -> IO ()
runOrBomb cmd args = do
  (c, out, err) <- readProcessWithExitCode cmd args ""
  case c of
    ExitSuccess -> return ()
    ExitFailure e -> do
      hPutStrLn stderr $ "Command \"" ++ unwords (cmd:args) ++ "\" failed with exit code: " ++ show e
      hPutStrLn stdout $ out
      hPutStrLn stderr $ err
      exitWith $ ExitFailure e

getConfigDist :: ConfigFlags -> IO FilePath
getConfigDist flags = do
  let Flag relDistDir = Flag defaultDistPref `mappend` configDistPref flags
  canonicalizePath relDistDir

getBuildDist :: BuildFlags -> IO FilePath
getBuildDist flags = do
  let Flag relDistDir = Flag defaultDistPref `mappend` buildDistPref flags
  canonicalizePath relDistDir

createDirectory' dir = do
  exists <- doesDirectoryExist $ dir
  unless exists $ createDirectory dir -- let's hope nobody creates the directory first!

configureMpfr :: FilePath -> IO ()
configureMpfr distDir =
  inDirectory mpfrRoot $ do
    putStrLn $ "--> Configuring MPFR..."
    runOrBomb "sh" ["configure", "--with-gmp=/usr/local", "--prefix=" ++ distDir]

makeMpfr :: FilePath -> IO ()
makeMpfr distDir =
  inDirectory mpfrRoot $ do
    putStrLn $ "--> Building MPFR..."
    runOrBomb "make" ["-j4"]
    runOrBomb "make" ["install"]

mpfrHooks :: UserHooks
mpfrHooks = autoconfUserHooks
    { preConf   = mpfrPreConf
    , postConf  = mpfrPostConf
    , confHook  = mpfrConfHook
    , preBuild  = mpfrPreBuild
    , buildHook = mpfrBuildHook
    , postBuild = mpfrPostBuild
    , postClean = mpfrPostClean
--    , sDistHook = mpfrSDist
    }
  where
  mpfrConfHook (pkg, pbi) flags = do
    --distDir <- getConfigDist flags
    mpfrDist <- getMpfrDist
    lbi <- confHook autoconfUserHooks (pkg, pbi) flags
    let lpd = localPkgDescr lbi
        lib = fromJust (library lpd)
        libbi = libBuildInfo lib
        libbi' = libbi { extraLibDirs = (mpfrDist </> "lib") : extraLibDirs libbi }
        lib' = lib { libBuildInfo = libbi' }
        lpd' = lpd { library = Just lib' }
    return lbi { localPkgDescr = lpd' }
 
  -- We need to create the "include" directory at some point, but we're doing it this early to make cabal
  -- shut up about it not being present.
  mpfrPreConf args flags = do
    distDir <- getConfigDist flags
    mpfrDist <- getMpfrDist
    createDirectory' $ mpfrDist </> "include"
    createDirectory' $ mpfrDist </> "lib"
    createDirectory' $ distDir </> "tmp"
    createDirectory' $ distDir </> "tmp_p"
    return emptyHookedBuildInfo

  mpfrPostConf args flags pkg_descr lbi = do
    postConf simpleUserHooks args flags pkg_descr lbi
    distDir <- getConfigDist flags
    mpfrDist <- getMpfrDist
    configureMpfr mpfrDist

    
  mpfrPreBuild args flags = do
    preBuild simpleUserHooks args flags
    distDir <- getBuildDist flags
    mpfrDist <- getMpfrDist
    makeMpfr mpfrDist
    
    let modified = emptyBuildInfo { extraLibs = ["mpfrPIC"]
                                  , extraLibDirs = [distDir </> "libtmp"]
                                  , includeDirs = [mpfrDist </> "include"]
                                  }

    return (Just modified, snd emptyHookedBuildInfo)

  mpfrBuildHook pkg_descr lbi hooks flags = do
    distDir <- getBuildDist flags
    mpfrDist <- getMpfrDist
    (ar, _) <- requireProgram silent arProgram defaultProgramDb
    let lbi' = lbi { withPrograms = updateProgram ar (withPrograms lbi) }

    putStrLn $ "Determining MPFR constants..."
    programExists <- doesFileExist $ mpfrDist </> "mkMpfrDerivedConstants"
    unless programExists $ do
      Just gcc <- programFindLocation gccProgram normal defaultProgramSearchPath
      runOrBomb gcc ["cbits/mkMpfrDerivedConstants.c", "-I" ++ mpfrDist </> "include", "-o", mpfrDist </> "mkMpfrDerivedConstants"]
    headerExists <- doesFileExist $ mpfrDist </> "include" </> "MpfrDerivedConstants.h"
    unless headerExists $ do
      header <- readProcess (mpfrDist </> "mkMpfrDerivedConstants") [] ""
      writeFile (mpfrDist </> "include" </> "MpfrDerivedConstants.h") header

    createDirectory' $ distDir </> "libtmp"
    picObjects <- pathsWithSuffix ".o" $ mpfrRoot </> "src" </> ".libs"
    createArLibArchive silent lbi' (distDir </> "libtmp" </> "libmpfrPIC.a") picObjects
    runOrBomb "ranlib" [distDir </> "libtmp" </> "libmpfrPIC.a"]

    buildHook simpleUserHooks pkg_descr lbi' hooks flags

  mpfrPostBuild args flags pkg_descr lbi = do
    distDir <- getBuildDist flags
    mpfrDist <- getMpfrDist
    (ar, _) <- requireProgram silent arProgram defaultProgramDb
--    (ranlib, _) <- requireProgram silent ranlibProgram defaultProgramDb
    let lbi' = lbi { withPrograms = updateProgram ar (withPrograms lbi) }

    putStrLn "Mangling static library..."
    inDirectory (distDir </> "tmp") $ do
      runOrBomb "ar" ["-x", distDir </> "build" </> "libHShaskell-mpfr-0.1.a"]
      runOrBomb "ar" ["-x", mpfrDist </> "lib" </> "libmpfr.a"]

    objects <- pathsWithSuffix ".o" $ distDir </> "tmp"
    --forM_ objects $ \o -> do
    --  runOrBomb "mv" [o, o <.> "tmp"]
    --  runOrBomb "objcopy" ["--redefine-syms=haskell-mpfr.rename", o <.> "tmp", o]

    createArLibArchive silent lbi' (distDir </> "build" </> "libHShaskell-mpfr-0.1.a") objects
    runOrBomb "ranlib" [distDir </> "build" </> "libHShaskell-mpfr-0.1.a"]

    profExists <- doesFileExist $ distDir </> "build" </> "libHShaskell-mpfr-0.1_p.a"
    when profExists $ do
      putStrLn "Mangling static library (prof)..."
      inDirectory (distDir </> "tmp_p") $ do
        runOrBomb "ar" ["-x", distDir </> "build" </> "libHShaskell-mpfr-0.1_p.a"]
        runOrBomb "ar" ["-x", mpfrDist </> "lib" </> "libmpfr.a"]

      objects <- pathsWithSuffix "o" $ distDir </> "tmp_p"
      --forM_ objects $ \o -> do
      --  runOrBomb "mv" [o, o <.> "tmp_p"]
      --  runOrBomb "objcopy" ["--redefine-syms=haskell-mpfr.rename", o <.> "tmp_p", o]

      createArLibArchive silent lbi' (distDir </> "build" </> "libHShaskell-mpfr-0.1_p.a") objects
      runOrBomb "ranlib" [distDir </> "build" </> "libHShaskell-mpfr-0.1_p.a"]

    postBuild simpleUserHooks args flags pkg_descr lbi

  mpfrPostClean args flags pkg_descr _ = do
    inDirectory mpfrRoot (readProcessWithExitCode "make" ["distclean"] "")
    return ()

{-  mpfrSDist pkg_descr mlbi hooks flags = do
-}
main :: IO ()
main = defaultMainWithHooks mpfrHooks
