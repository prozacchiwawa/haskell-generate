{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.IORef
import Data.List ( nub )
import Data.Version ( showVersion, makeVersion )
import Distribution.Package ( PackageId, PackageIdentifier (..), UnitId, unPackageName, mkPackageName, pkgName, pkgVersion )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..), hsSourceDirs, libBuildInfo, buildInfo)
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks, versionNumbers )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, withExeLBI, ComponentLocalBuildInfo(), LocalBuildInfo(), componentPackageDeps )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag, buildDistPref, defaultDistPref, fromFlagOrDefault )
import Distribution.Simple.Utils ( rewriteFileEx, createDirectoryIfMissingVerbose )
import Distribution.Types.MungedPackageId ( mungedName, mungedVersion )
import Distribution.Types.MungedPackageName ( unMungedPackageName )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Verbosity ( Verbosity, normal )
import System.Directory ( canonicalizePath )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi flags
     buildHook simpleUserHooks pkg lbi hooks flags
  }

--  Very ad-hoc implementation of difference lists
singletonDL :: a -> [a] -> [a]
singletonDL = (:)

emptyDL :: [a] -> [a]
emptyDL = id

appendDL :: ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]
appendDL x y = x . y

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> BuildFlags -> IO ()
generateBuildModule verbosity pkg lbi flags = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withTestLBI pkg lbi $ \suite suitelbi -> do
    srcDirs <- mapM canonicalizePath $ hsSourceDirs $ testBuildInfo suite
    distDir <- canonicalizePath $ fromFlagOrDefault defaultDistPref $ buildDistPref flags

    depsVar <- newIORef emptyDL
    withLibLBI pkg lbi $ \lib liblbi ->
      modifyIORef depsVar $ appendDL . singletonDL $ depsEntry (libBuildInfo lib) liblbi suitelbi
    withExeLBI pkg lbi $ \exe exelbi ->
      modifyIORef depsVar $ appendDL . singletonDL $ depsEntry (buildInfo exe) exelbi suitelbi
    deps <- fmap ($ []) $ readIORef depsVar

    rewriteFileEx normal (map fixchar $ dir </> "Build_" ++ uqTestName suite ++ ".hs") $ unlines 
      [ "module Build_" ++ map fixchar (uqTestName suite) ++ " where"
      , "getDistDir :: FilePath"
      , "getDistDir = " ++ show distDir
      , "getSrcDirs :: [FilePath]"
      , "getSrcDirs = " ++ show srcDirs
      , "deps :: [([FilePath], [String])]"
      , "deps = " ++ show deps
      ]

  where
    formatdeps = map (formatone . snd)
    formatone p = (unPackageName $ pkgName p) ++ "-" ++ showVersion (vPackageVersion p)
    depsEntry targetbi targetlbi suitelbi = (hsSourceDirs targetbi, formatdeps $ testDeps targetlbi suitelbi)
    fixchar '-' = '_'
    fixchar c = c
    uqTestName p = unUnqualComponentName $ testName p
    vPackageVersion p = makeVersion $ versionNumbers $ pkgVersion p

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, PackageId)]
testDeps xs ys = cvt <$> nub (componentPackageDeps xs ++ componentPackageDeps ys)
  where
    cvt (unit,mpd) =
      ( unit
      , PackageIdentifier (mkPackageName $ unMungedPackageName $ mungedName mpd) $ mungedVersion mpd
      )
