module Handler.Lists where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Data.List as L

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

filterCompletesOut (num, l) = do
  if num < 100
    then
      True
    else
      False

getLists :: Text -> Text -> Bool -> Bool -> HandlerT App IO Html
getLists category username doFilter showComplete = do
  lists <- runDB $ selectList (if doFilter then [ListOwner ==. username, ListCategory ==. category] else [ListCategory ==. category]) [Desc ListId]
  (newListForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm (listForm category username)
  users <- runDB $ selectList [] [Asc UserName]
  let filterName = if doFilter then username else "All"
  let pageName = T.unpack $ "Lists"
  let postRoute = ListsR
  lists' <- mapM getCompletionValue lists
  let lists2 = if showComplete then lists' else filter filterCompletesOut lists'
  cats <- runDB $ selectList [] [Desc ListId]
  let categories = L.nubBy (\(Entity _ x) (Entity _ y) -> listCategory x == listCategory y) cats
  defaultLayout $ do
    setTitle "Lists"
    $(widgetFile "lists")

getListsR :: Text -> Handler Html
getListsR category = do
  (Entity _ user) <- requireAuth
  getLists category (userName user) False False

getListsFilteredR :: Text -> Bool -> Handler Html
getListsFilteredR category showComplete = do
  (Entity _ user) <- requireAuth
  getLists category (userName user) False showComplete

getListsForUserR :: Text -> Text -> Handler Html
getListsForUserR category username = getLists category username True False

getListsFilteredForUserR :: Text -> Text -> Bool -> Handler Html
getListsFilteredForUserR category username showComplete = getLists category username True showComplete

postListsR :: Text -> Handler Html
postListsR category = do
  (Entity _ user) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (listForm category $ userName user)
  case result of
    FormSuccess list -> do
      nid <- runDB $ insert list
      redirect (ListR nid)
    _                -> redirect $ ListsR category
