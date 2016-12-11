module Handler.AjaxItem where

import Import

postAjaxItemR :: ItemId -> Handler Value
postAjaxItemR itemId = do
  maybeItem <- runDB $ get itemId
  case maybeItem of
    Nothing -> returnJson False
    Just item -> do
      _ <- runDB $ update itemId [ItemStatus =. (if itemStatus item then False else True)]
      returnJson True

deleteAjaxItemR :: ItemId -> Handler RepPlain
deleteAjaxItemR itemId = do
  runDB $ delete itemId
  return $ RepPlain ""
