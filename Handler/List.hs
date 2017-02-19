module Handler.List where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import Data.List (maximumBy)
import qualified Data.Text as T

itemForm :: ListId -> Int -> AForm Handler Item
itemForm listId order = Item
  <$> areq textField (bfs ("Text" :: Text)) Nothing
  <*> areq hiddenField "" (Just False)
  <*> areq hiddenField "" (Just listId)
  <*> areq hiddenField "" (Just order)

getListR :: ListId -> Handler Html
getListR listId = do
  maybeList <- runDB $ get listId
  case maybeList of
    Nothing -> defaultLayout [whamlet|<p> No such List !|]
    Just list -> do
      items <- runDB $ selectList [ItemList ==. listId] [Asc ItemOrder, Asc ItemId]
      let maxOrder = if null items
                      then
                        0
                      else
                        itemOrder . entityVal $ Data.List.maximumBy (comparing $ itemOrder . entityVal) items
      (newItemForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm (itemForm listId (maxOrder + 1))
      defaultLayout $ do
        setTitle . fromString $ T.unpack $ "CSTodo - " ++ (listName list)
        $(widgetFile "list")

postListR :: ListId -> Handler Html
postListR listId = do
  ((result, _), _) <- runFormPost $ renderDivs (itemForm listId 0)
  case result of
    FormSuccess item -> do
      _ <- runDB $ insert item
      redirect (ListR listId)
    _ -> redirect HomeR

deleteListR :: ListId -> Handler RepPlain
deleteListR listId = do
  runDB $ deleteCascade listId
  return $ RepPlain ""

getListCopyR :: ListId -> Text -> Text -> Handler Text
getListCopyR listId newName newCat = do
  Entity _ user <- requireAuth
  items <- runDB $ selectList [ItemList ==. listId] [Asc ItemId]
  let nList = List { listName = newName, listOwner = userName user, listCategory = newCat }
  nid <- runDB $ insert nList
  forM_ items $ \(Entity _ item) -> do
    _ <- runDB $ insert $ item { itemList = nid }
    return ()
  r <- getUrlRender
  return $ r $ ListR nid
