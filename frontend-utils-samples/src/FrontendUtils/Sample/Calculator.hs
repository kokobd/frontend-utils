module FrontendUtils.Sample.Calculator
  ( main
  ) where

import           FrontendUtils.Component.Button (buttonAttr)
import           FrontendUtils.DevUtil          (GhcJsmConfig (WarpConfig), JSM,
                                                 runJsm)
import           FrontendUtils.WebApp           (WebApp (..), materialize',
                                                 runWebApp)
import           Reflex.Dom
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let (portStr, rootDir) =
          case args of
            [portStr', rootDir'] -> (portStr', rootDir')
            _                    -> ("8080", "")
    let port = read portStr
    runJsm
      (WarpConfig port rootDir)
      jsmMain

jsmMain :: JSM ()
jsmMain = runWebApp
    $ materialize' "styles/all.css"
    $ WebApp headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = blank

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h1" $ text "Calculator"
    el "div" $ do
      (evt, _) <- buttonAttr ("class" =: "waves-effect waves-light btn") $ text "Click Me"
      display =<< count evt
    divClass "card blue-grey darken-1" $ do
      divClass "card-content white-text" $ do
        elClass "span" "card-title" $ text "Card Title" 
        el "p" $ text "A very simple card"
      divClass "card-action" $
        elAttr "a" ("href" =: "#") $ text "A link"
