{-# LANGUAGE RecordWildCards #-}

module Model.Empty.Internal (
  getEmptysM,
  getEmptyM,
  insertEmptyM,
  updateEmptyM,
  deleteEmptyM
) where



import           Api.Params
import           Api.Response
import           Import
import           LN.T.Empty
import           Model.Empty.Function



getEmptysM :: UserId -> Handler [Entity Empty]
getEmptysM _ = do
  selectListDb' [] [] EmptyId



getEmptyM :: UserId -> EmptyId -> Handler (Entity Empty)
getEmptyM _ empty_id = do
  notFoundMaybe =<< selectFirstDb [ EmptyId ==. empty_id ] []



insertEmptyM :: UserId -> EmptyRequest -> Handler (Entity Empty)
insertEmptyM user_id empty_request = do

  ts <- timestampH'

  let
    empty' = (emptyRequestToEmpty user_id empty_request) { emptyCreatedAt = Just ts }

  insertEntityDb empty'



updateEmptyM :: UserId -> EmptyId -> EmptyRequest -> Handler (Entity Empty)
updateEmptyM user_id empty_id empty_request = do

  ts <- timestampH'

  let
    Empty{..} = (emptyRequestToEmpty user_id empty_request) { emptyModifiedAt = Just ts }

  updateWhereDb
    [ EmptyUserId ==. user_id, EmptyId ==. empty_id ]
    [ EmptyModifiedAt =. emptyModifiedAt
    , EmptyValue =. emptyValue
    ]

  notFoundMaybe =<< selectFirstDb [ EmptyUserId ==. user_id, EmptyId ==. empty_id ] []



deleteEmptyM :: UserId -> EmptyId -> Handler ()
deleteEmptyM user_id empty_id = do
  deleteWhereDb [ EmptyUserId ==. user_id, EmptyId ==. empty_id ]
