module LN.Lifted (
  _requireAuthId,
  _runDB
) where



import Import
import LN.Control



_requireAuthId :: YesodAuth master => LN.ControlMA (HandlerT master IO) (AuthId master)
_requireAuthId = lift requireAuthId



_runDB :: YesodPersist site => YesodDB site a -> LN.ControlMA (HandlerT site IO) a
_runDB op = lift $ runDB op
