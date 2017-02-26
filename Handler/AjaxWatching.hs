module Handler.AjaxWatching where

import Import

postAjaxWatchingR :: ListId -> Handler Value
postAjaxWatchingR listId = do
  maybeList <- runDB $ get listId
  case maybeList of
    Nothing -> returnJson False
    Just _ -> do
      val <- runInputPost $ iopt textField "watching"
      _ <- runDB $ update listId [ListWatching =. val]
      returnJson True

