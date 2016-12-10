module Handler.TemplateCopy where

import Import


getTemplateCopyR :: ListId -> Text -> Handler Html
getTemplateCopyR listId newName = do
  Entity _ user <- requireAuth
  items <- runDB $ selectList [ItemList ==. listId] [Asc ItemId]
  let nList = List { listName = newName, listOwner = userName user, listIsTemplate = False }
  nid <- runDB $ insert nList
  forM_ items $ \(Entity _ item) -> do
    _ <- runDB $ insert $ item { itemList = nid }
    return ()
  redirect $ ListR nid
