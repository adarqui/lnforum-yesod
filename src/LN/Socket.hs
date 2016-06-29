{-# LANGUAGE OverloadedStrings #-}

module LN.Socket (
  socketHub
) where



import           LN.Import
import           Yesod.WebSockets



socketHub :: WebSocketsT Handler ()
socketHub = do
  sendTextData ("Welcome" :: Text)
