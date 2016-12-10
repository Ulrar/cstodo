module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Welcome To Yesod!"
  $(widgetFile "homepage")

