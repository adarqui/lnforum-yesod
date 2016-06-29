module LN.Lifted (
  _requireAuthId,
  _runDB
) where



import Import
import Control



_requireAuthId :: YesodAuth master => ControlMA (HandlerT master IO) (AuthId master)
_requireAuthId = lift requireAuthId



_runDB :: YesodPersist site => YesodDB site a -> ControlMA (HandlerT site IO) a
_runDB op = lift $ runDB op
