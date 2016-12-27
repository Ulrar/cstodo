module Handler.Lists where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql
import qualified Data.Text as T

listForm :: Text -> AForm Handler List
listForm username = List
  <$> areq textField (bfs ("Name" :: Text)) Nothing
  <*> areq hiddenField "" (Just username)
  <*> areq hiddenField "" (Just False)

getCompletionValue (Entity lid list) = do
  total <- runDB $ count [ItemList ==. lid]
  numDone <- runDB $ count [ItemStatus ==. True, ItemList ==. lid]
  return ((fromIntegral numDone) / (fromIntegral total) * 100, (Entity lid list))

getLists :: Text -> Bool -> HandlerT App IO Html
getLists username doFilter = do
  lists <- runDB $ selectList (if doFilter then [ListOwner ==. username] else [ListIsTemplate ==. False]) [Desc ListId]
  (newListForm, enctype) <- generateFormPost $ renderBootstrap3 BootstrapInlineForm (listForm username)
  users <- runDB $ selectList [UserName !=. "template"] [Asc UserName]
  let filterName = if doFilter then username else "All"
  let pageName = T.unpack $ "Lists"
  let postRoute = ListsR
  lists2 <- mapM getCompletionValue lists
  defaultLayout $ do
    setTitle "Lists"
    $(widgetFile "lists")

getListsR :: Handler Html
getListsR = do
  (Entity _ user) <- requireAuth
  getLists (userName user) False

getListsForUserR :: Text -> Handler Html
getListsForUserR username = getLists username True

postListsR :: Handler Html
postListsR = do
  (Entity _ user) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (listForm $ userName user)
  case result of
    FormSuccess list -> do
      nid <- runDB $ insert list
      redirect (ListR nid)
    _                -> redirect ListsR
