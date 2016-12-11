module Handler.List where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import qualified Data.Text as T

itemForm :: ListId -> AForm Handler Item
itemForm listId = Item
  <$> areq textField (bfs ("Text" :: Text)) Nothing
  <*> areq hiddenField "" (Just False)
  <*> areq hiddenField "" (Just listId)

getListR :: ListId -> Handler Html
getListR listId = do
  maybeList <- runDB $ get listId
  case maybeList of
    Nothing -> defaultLayout [whamlet|<p> No such List !|]
    Just list -> do
      items <- runDB $ selectList [ItemList ==. listId] [Asc ItemId]
      (newItemForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm (itemForm listId)
      defaultLayout $ do
        setTitle . fromString $ T.unpack $ "CSTodo - " ++ (listName list)
        $(widgetFile "list")

postListR :: ListId -> Handler Html
postListR listId = do
  ((result, _), _) <- runFormPost $ renderDivs (itemForm listId)
  case result of
    FormSuccess item -> do
      _ <- runDB $ insert item
      redirect (ListR listId)
    _ -> redirect ListsR

deleteListR :: ListId -> Handler RepPlain
deleteListR listId = do
  runDB $ deleteCascade listId
  return $ RepPlain ""
