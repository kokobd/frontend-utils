{-# LANGUAGE CPP #-}

module FrontendUtils.DevUtil
  ( runJsm
  , JSM
  , GhcJsmConfig(..)
  ) where

#ifdef ghcjs_HOST_OS
#else
import           FrontendUtils.DevUtil.GHC         (runJsmWarp)
#endif
import           Language.Javascript.JSaddle.Types (JSM)

data GhcJsmConfig
  -- | port, root folder to static resources
  = WarpConfig Int FilePath

{-| Converts @JSM ()@ to @IO ()@.

* If the compiler is GHCJS, 'JSM' becomes 'IO' automatically.
* If the compiler is GHC, use the provided config to convert the @JSM ()@
-}
runJsm :: GhcJsmConfig -- ^config how the 'JSM' monad is converted when compiled by GHC
       -> JSM ()
       -> IO ()
#ifdef ghcjs_HOST_OS
runJsm _                         = id
#else
runJsm (WarpConfig port rootDir) = runJsmWarp port rootDir
#endif
