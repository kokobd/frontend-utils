{-# LANGUAGE CPP              #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module FrontendUtils.DevUtil.Cabal
  (
  -- *Types
    BuildConfig(..)
  , buildConfig_buildDir
  , buildConfig_verbosity
  , buildConfig_optimisationLevel
  , Verbosity
  , OptimisationLevel(..)
  , BuildM
  , WebResBuildConfig(..)
  , webResBuildConfig_base
  , webResBuildConfig_exeName
  , webResBuildConfig_ghcResDir
  , WebResBuildM
  , liftBuildM
  , runWebResBuild
  -- *Main
  , mainWithCustomBuild
  -- *Pre-defined operations
  , getResourceRoot
  , copyFiles
  , copyWebResFiles
  , buildSass
  ) where

import           Control.Lens                      hiding ((<.>))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Distribution.Simple               (UserHooks (..),
                                                    defaultMainWithHooks,
                                                    simpleUserHooks)
import           Distribution.Simple.Compiler      (OptimisationLevel (..))
import           Distribution.Simple.Setup         (buildVerbosity,
                                                    fromFlagOrDefault)
import           Distribution.Simple.Utils         (rawSystemExit)
import qualified Distribution.Simple.Utils         as Cabal (copyFiles)
import qualified Distribution.Types.LocalBuildInfo as LocalBuildInfo
import           Distribution.Verbosity            (Verbosity)
import qualified Distribution.Verbosity            as Verbosity
import           System.Directory                  (getCurrentDirectory,
                                                    setCurrentDirectory)
import           System.FilePath                   ((-<.>), (<.>), (</>))

data BuildConfig = BuildConfig
  { _buildConfig_buildDir          :: FilePath
  , _buildConfig_verbosity         :: Verbosity
  , _buildConfig_optimisationLevel :: OptimisationLevel
  }

type BuildM a = ReaderT BuildConfig IO a

data WebResBuildConfig = WebResBuildConfig
  { _webResBuildConfig_base      :: BuildConfig
  , _webResBuildConfig_exeName   :: FilePath
  , _webResBuildConfig_ghcResDir :: FilePath
  }

type WebResBuildM a = ReaderT WebResBuildConfig IO a

makeLenses ''BuildConfig
makeLenses ''WebResBuildConfig

mainWithCustomBuild :: BuildM () -> IO ()
mainWithCustomBuild buildM = defaultMainWithHooks $ simpleUserHooks
    { buildHook = buildHook'
    }
  where
    buildHook' pkgDescr localBi hooks flags = do
      buildHook simpleUserHooks pkgDescr localBi hooks flags
      let buildDir = LocalBuildInfo.buildDir localBi
      let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity flags)
      let optimisationLevel = LocalBuildInfo.withOptimization localBi

      let buildConfig = BuildConfig buildDir verbosity optimisationLevel
      runReaderT buildM buildConfig

runWebResBuild :: FilePath -- ^executable name
               -> FilePath -- ^resource subdir when compiled by GHC
               -> WebResBuildM a
               -> BuildM a
runWebResBuild exeName ghcResDir m = do
    buildConfig <- ask
    liftIO $ runReaderT m (WebResBuildConfig buildConfig exeName ghcResDir)

liftBuildM :: BuildM a -> WebResBuildM a
liftBuildM m = do
    buildConfig <- view webResBuildConfig_base
    liftIO $ runReaderT m buildConfig

getResourceRoot :: WebResBuildM FilePath
getResourceRoot = do
    buildDir <- view (webResBuildConfig_base . buildConfig_buildDir)
    exeName <- view webResBuildConfig_exeName
    resSubDir <- view webResBuildConfig_ghcResDir
    pure $ webResourceRoot buildDir exeName resSubDir

webResourceRoot :: FilePath -- ^build dir
                -> FilePath -- ^executable name
                -> FilePath -- ^resource sub dir (when compiled by GHC)
                -> FilePath -- ^combined path representing web resource root
#ifdef ghcjs_HOST_OS
webResourceRoot buildDir exeName _ =
    buildDir </> exeName </> (exeName ++ ".jsexe")
#else
webResourceRoot buildDir exeName ghcResSubDir =
    buildDir </> exeName </> ghcResSubDir
#endif

-- | Works like 'Cabal.copyFiles', with build directory prepended to dest path
copyFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> BuildM ()
copyFiles v destDir sources = do
    buildDir <- view buildConfig_buildDir
    liftIO $ Cabal.copyFiles v (buildDir </> destDir) sources

copyWebResFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> WebResBuildM ()
copyWebResFiles verbosity destDir sources = do
    webResRoot <- getResourceRoot
    liftIO $ Cabal.copyFiles verbosity (webResRoot </> destDir) sources

buildSass :: FilePath -- ^sass sources dir
          -> FilePath -- ^value of @--load-path@
          -> FilePath -- ^sass dest dir (relative to web resource root)
          -> FilePath -- ^bundle file (e.g. "all.css")
          -> WebResBuildM ()
buildSass sourceDir includeDir destDir bundleFile = do
    webResRoot <- getResourceRoot
    let destDir' = webResRoot </> destDir
    -- build sass sources
    liftIO $ rawSystemExit Verbosity.normal "sass"
      [ "--no-source-map"
      , "--update"
      , "--load-path=" <> includeDir
      , sourceDir <> ":" <> destDir'
      ]
    -- minimize the bundled CSS file
    optimisationLevel <- view (webResBuildConfig_base . buildConfig_optimisationLevel)
    if optimisationLevel == MaximumOptimisation
    then
      withCurrentDirectory' destDir' $
        liftIO $ rawSystemExit Verbosity.normal "cleancss"
          [ "-O2"
          , "--output"
          , bundleFile -<.> "min" <.> "css"
          , bundleFile
          ]
    else pure ()

withCurrentDirectory' :: MonadIO m => FilePath -> m a -> m a
withCurrentDirectory' tempCurrentDirectory action = do
  currentDirectory <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory tempCurrentDirectory
  result <- action
  liftIO $ setCurrentDirectory currentDirectory
  pure result
