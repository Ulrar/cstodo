module Handler.Item where

import Import

deleteItemR :: ItemId -> Handler RepPlain
deleteItemR itemId = do
  runDB $ delete itemId
  return $ RepPlain ""
