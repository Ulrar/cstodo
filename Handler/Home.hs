module Handler.Home where

import Import
--import Yesod.Form.Bootstrap3
import Yesod.Auth.HashDB

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "CSTodo"
  $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  (Entity uid user) <- requireAuth
  pass1 <- runInputPost $ ireq textField "password1"
  pass2 <- runInputPost $ ireq textField "password2"
  if pass1 == pass2
    then do
      user' <- setPassword pass1 user
      runDB $ replace uid user'
      setMessage "Success"
      redirect HomeR
    else do
      setMessage "Password didn't match"
      redirect HomeR
