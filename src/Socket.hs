{-# LANGUAGE OverloadedStrings #-}

module Socket (
  socketHub
) where


import Import
-- import Yesod.Core
import Yesod.WebSockets
import Conduit


socketHub :: WebSocketsT Handler ()
socketHub = do
  sendTextData ("Welcome" :: Text)
