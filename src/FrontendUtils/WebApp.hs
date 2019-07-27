{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module FrontendUtils.WebApp
  ( WebApp(..)
  , webApp_head
  , webApp_body
  , runWebApp
  -- *Basic combinators
  , viewPortMobile
  , materialize
  , linkCss
  -- *Predefined combinations
  , materialize'
  ) where

import           Control.Lens
import           Data.Functor                      (void)
import           Data.Text                         (Text)
import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex.Dom                        hiding (mainWidgetWithHead)
import           Reflex.Dom.Main                   (mainWidgetWithHead)
import qualified Reflex.Dom.Main                   as Reflex

data WebApp m n = WebApp
  { _webApp_head :: m ()
  , _webApp_body :: n ()
  }

makeLenses ''WebApp

runWebApp :: (forall x1 x2. WebApp (Reflex.Widget x1) (Reflex.Widget x2)) -> JSM ()
runWebApp app = mainWidgetWithHead (app ^. webApp_head) (app ^. webApp_body)

instance (Monad m, Monad n) => Semigroup (WebApp m n) where
  WebApp head1 body1 <> WebApp head2 body2 =
    WebApp (head1 >> head2) (body1 >> body2)

instance (Monad m, Monad n) => Monoid (WebApp m n) where
  mempty = WebApp (pure ()) (pure ())

viewPortMobile :: (MonadWidget t m, MonadWidget t2 n) => WebApp m n -> WebApp m n
viewPortMobile = (<> WebApp viewPort' (pure ()))
  where
    -- Optimize for mobile device
    viewPort' = elAttr "meta" ("name" =: "viewport"
      <> "content" =: "width=device-width, initial-scale=1.0") blank

materialize :: (MonadWidget t m, MonadWidget t2 n) => WebApp m n -> WebApp m n
materialize (WebApp head_ body_) = WebApp head' body'
  where
    head' = do
      head_
      -- Material Icons
      elAttr "link"
        ( "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons"
       <> "rel" =: "stylesheet"
        ) blank
    body' = do
      body_
      elAttr "link" ("type" =: "text/css"
        <> "rel" =: "stylesheet"
        <> "href" =: "styles/all.css"
        <> "media" =: "screen,projection") blank

linkCss :: (MonadWidget t m, MonadWidget t2 n)
        => Text
        -> WebApp m n
        -> WebApp m n
linkCss cssUrl = over webApp_head addLinkCss
  where
    addLinkCss m = do
      void m
      elAttr "link" ("type" =: "text/css"
        <> "rel" =: "stylesheet"
        <> "href" =: cssUrl
        <> "media" =: "screen,projection") blank

{-|Complete combinator set for a simple materialized app.
Based on 'materialize', with 'viewPortMobile', 'linkCss' included.
-}
materialize' :: (MonadWidget t m, MonadWidget t2 n)
             => Text -- ^css bundle url
             -> WebApp m n
             -> WebApp m n
materialize' cssUrl = linkCss cssUrl . materialize . viewPortMobile
