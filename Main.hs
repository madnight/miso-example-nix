{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest
import           Miso                          hiding (defaultOptions)
import           Miso.String
import Prelude hiding (head, concat)
import Data.List hiding (intercalate, concat)
import Control.Monad

-- | Model
newtype Model =
  Model { info :: Maybe APIInfo
        } deriving (Eq, Show)

-- | Action
data Action
  = FetchGitHub MisoString
  | SetGitHub APIInfo
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = startApp
           App { model = Model Nothing
               , initialAction = NoOp
               , mountPoint = Nothing
               , ..
               }
    where
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model

updateModel (FetchGitHub s) m = m <# do
  SetGitHub <$> getGitHubAPIInfo s

updateModel (SetGitHub apiInfo) m =
  noEff m { info = Just apiInfo }

updateModel NoOp m = noEff m { info = Nothing }

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view = div_ [ style_ $ M.fromList [
                  (pack "text-align", pack "center")
                , (pack "margin", pack "100px")
                ]
               ] [
        h1_ [class_ $ pack "title" ] [ text $ pack "Haskell IRC Log Search" ]

      , input_ attrs [
          text $ pack "Fetch JSON from https://api.github.com via XHR"
          ]

      , case info of
          Nothing -> div_ [] [ text $ pack "" ]
          Just APIInfo{..} ->
            div_ [] [
               br_ [] []
               ,
               table_ [ class_ $ pack "table is-striped" ] [
                 thead_ [] [
                   tr_ [] [
                     th_ [] [ text $ pack "Search Results"]
                   ]
                 ]
               , tbody_ [] $ results_ rows
               ]
               ]
            ]

      where
        attrs = [ onKeyDown $ \case
          EnterButton -> FetchGitHub "  "
          _           -> NoOp
                , onInput $ FetchGitHub
                , class_ $ pack "button is-large is-outlined"
                ] ++ [ disabled_ False | isJust info ]


pattern EnterButton :: KeyCode
pattern EnterButton = KeyCode 13

results_ :: [[MisoString]] -> [View action]
results_ m = [ tr_ [] [ td_ [] [ text $ intercalate " " $ m !! i ] ] | i <- [1..10] ]

data APIInfo
  = APIInfo
  { database :: MisoString
  , rows :: [[MisoString]]
  } deriving (Show, Eq, Generic)

instance FromJSON APIInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo '_' }

getGitHubAPIInfo :: MisoString -> IO APIInfo
getGitHubAPIInfo q = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String APIInfo of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack $ "http://localhost:8001/irc-logs-7f641b3.json?sql=select+*+from+haskell+where+post+like+%22%25" ++ (fromMisoString q) ++ "%25%22"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
