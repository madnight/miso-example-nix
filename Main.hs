{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Main where

import Network.URI
import Data.Proxy
import Servant.API
#if MIN_VERSION_servant(0,10,0)
import Servant.Utils.Links
#endif

import Miso

-- | Model
data Model
  = Model
  { uri :: URI
    -- ^ current URI of application
  } deriving (Eq, Show)

-- | HasURI typeclass
instance HasURI Model where
  lensURI = makeLens getter setter
    where
      getter = uri
      setter = \m u -> m { uri = u }

-- | Action
data Action
  = HandleURI URI
  | ChangeURI URI
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  currentURI <- getCurrentURI
  {- let currentURI = uriparser $ parseURI "http://localhost:23322/#/about" -}
  print currentURI
  startApp App { model = Model currentURI, initialAction = NoOp, ..}
  where
    update = updateModel
    events = defaultEvents
    subs   = [ uriSub HandleURI ]
    view   = viewModel
    mountPoint = Nothing

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel model@Model {..}
    | uri == (uriparser $ parseURI "http:#/about") = about
    | otherwise = home
  where
    home = div_ [] [
        div_ [] [ text "home" ]
      , button_ [ onClick goAbout ] [ text "go about" ]
      ]
    about  = div_ [] [
        div_ [] [ text "about" ]
      , button_ [ onClick goHome ] [ text "go home" ]
      ]
    the404 = div_ [] [
        text "the 404 :("
      , button_ [ onClick goHome ] [ text "go home" ]
      ]

uriparser (Just x) = x

goto :: String -> Action
goto = ChangeURI . uriparser . parseRelativeReference

goAbout :: Action
goAbout = goto "#/about"

goHome  :: Action
goHome = goto "/"

