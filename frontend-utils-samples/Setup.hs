module Main
  ( main
  ) where

import           FrontendUtils.DevUtil.Cabal
import qualified Distribution.Verbosity as Verbosity
import System.FilePath ((</>))

webResSrc :: FilePath
webResSrc = "webres"

main :: IO ()
main = mainWithCustomBuild $
    runWebResBuild "calculator" webResSrc customBuild

customBuild :: WebResBuildM ()
customBuild = do
    copyWebResFiles Verbosity.normal "" [(webResSrc, "favicon.ico")]
    buildSass
      (webResSrc </> "styles")
      (webResSrc </> "extern/materialize/sass")
      "styles"
      "all.css"
    copyWebResFiles Verbosity.normal "js"
      [(webResSrc </> "extern/materialize/js/bin", "materialize.js")]
