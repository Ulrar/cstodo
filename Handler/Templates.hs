module Handler.Templates where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import qualified Data.Text as T

tplForm :: AForm Handler List
tplForm = List
  <$> areq textField (bfs ("Name" :: Text)) Nothing
  <*> areq hiddenField "" (Just "template")
  <*> areq hiddenField "" (Just True)

adaptToTemplate :: Num t => t1 -> (t, t1)
adaptToTemplate list = (0, list)

getTemplatesR :: Handler Html
getTemplatesR = do
  lists <- runDB $ selectList [ListIsTemplate ==. True] [Desc ListId]
  (newListForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm tplForm
  users <- runDB $ selectList [] [Asc UserName]
  let filterName = "" :: Text
  let pageName = T.unpack "Templates"
  let postRoute = TemplatesR
  let lists2 = map adaptToTemplate lists
  defaultLayout $ do
    setTitle "Templates"
    $(whamletFile "templates/lists.hamlet")

postTemplatesR :: Handler Html
postTemplatesR = do
  ((result, _), _) <- runFormPost $ renderDivs tplForm
  case result of
    FormSuccess list -> do
      nid <- runDB $ insert list
      redirect (ListR nid)
    _                -> redirect TemplatesR
