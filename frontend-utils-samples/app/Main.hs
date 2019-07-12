module Main
  ( main
  ) where

import           Data.Map.Strict       (Map)
import           Data.Text             (Text)
import           FrontendUtils.DevUtil
import           FrontendUtils.WebApp  (WebApp (..), appMaterialized, runWebApp)
import           Reflex.Dom
import           System.Environment

main :: IO ()
main = do
    [portStr, rootDir] <- getArgs
    let port = read portStr
    runJsm
      (WarpConfig port rootDir)
      jsmMain

jsmMain :: JSM ()
jsmMain = runWebApp
    $ appMaterialized "styles/all.css"
    $ WebApp headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = blank

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h1" $ text "Calculator"
    (evt, _) <- buttonAttr ("class" =: "waves-effect waves-light btn") $ text "Click Me"
    display =<< count evt

buttonAttr :: DomBuilder t m
           => Map Text Text -- ^attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure (domEvent Click e, x)
