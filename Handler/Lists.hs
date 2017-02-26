module Handler.Lists where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import qualified Data.Text as T

listForm :: Text -> Text -> AForm Handler List
listForm category username = List
  <$> areq textField (bfs ("Name" :: Text)) Nothing
  <*> areq hiddenField "" (Just username)
  <*> areq hiddenField "" (Just category)
  <*> areq hiddenField "" (Just Nothing)

getCompletionValue (Entity lid list) = do
  total <- runDB $ count [ItemList ==. lid]
  if total == 0
    then
      return (0, (Entity lid list))
    else do
      numDone <- runDB $ count [ItemStatus ==. True, ItemList ==. lid]
      return ((fromIntegral numDone) / (fromIntegral total) * 100, (Entity lid list))

getLists :: Text -> Text -> Bool -> HandlerT App IO Html
getLists category username doFilter = do
  lists <- runDB $ selectList (if doFilter then [ListOwner ==. username, ListCategory ==. category] else [ListCategory ==. category]) [Desc ListId]
  (newListForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm (listForm category username)
  users <- runDB $ selectList [] [Asc UserName]
  let filterName = if doFilter then username else "All"
  let pageName = T.unpack $ "Lists"
  let postRoute = ListsR
  lists2 <- mapM getCompletionValue lists
  defaultLayout $ do
    setTitle "Lists"
    $(widgetFile "lists")

getListsR :: Text -> Handler Html
getListsR category = do
  (Entity _ user) <- requireAuth
  getLists category (userName user) False

getListsForUserR :: Text -> Text -> Handler Html
getListsForUserR category username = getLists category username True

postListsR :: Text -> Handler Html
postListsR category = do
  (Entity _ user) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (listForm category $ userName user)
  case result of
    FormSuccess list -> do
      nid <- runDB $ insert list
      redirect (ListR nid)
    _                -> redirect $ ListsR category
