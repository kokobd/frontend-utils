module FrontendUtils.DevUtil.GHC
  ( runJsmWarp
  ) where

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets (jsaddleApp, jsaddleOr)
import qualified Network.Wai                            as Wai
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         ssMaxAge, staticApp)
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)
import           WaiAppStatic.Types                     (MaxAge (MaxAgeSeconds))

{-| Run JSM as warp application

The app will be served at given port. @/@, @/jsaddle.js@, @/sync/**@ are handled
by jsaddle-warp, while other URLs are handled by a file server with the given 
root folder.
-}
runJsmWarp :: Int -- ^port
           -> FilePath -- ^root folder to serve from
           -> JSM ()
           -> IO ()
runJsmWarp port dir jsm = do
    jsApp <- jsaddleOr defaultConnectionOptions (jsm >> syncPoint) jsaddleApp
    let webAppSettings = (defaultWebAppSettings dir)
          { ssMaxAge = MaxAgeSeconds 0
          }
        staticApp' = staticApp webAppSettings
    Warp.run port $ \request respond ->
      (case Wai.pathInfo request of
        []             -> jsApp
        ["jsaddle.js"] -> jsApp
        ("sync":_)     -> jsApp
        _              -> staticApp') request respond