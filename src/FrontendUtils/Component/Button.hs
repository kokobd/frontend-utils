module FrontendUtils.Component.Button
  ( buttonAttr
  ) where

import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Reflex.Dom

buttonAttr :: DomBuilder t m
           => Map Text Text -- ^attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure (domEvent Click e, x)

