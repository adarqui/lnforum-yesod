{-# LANGUAGE RecordWildCards #-}

module All.Reminder (
  -- Handler
  getRemindersR,
  postRemindersR,
  getReminderR,
  putReminderR,
  deleteReminderR,
  getReminderFoldersR,
  postReminderFoldersR,
  getReminderFolderR,
  postReminderFolderR,
  putReminderFolderR,
  deleteReminderFolderR,

  -- Model/Function
  reminderRequestToReminder,
  reminderToResponse,
  remindersToResponses,
  reminderFolderRequestToReminderFolder,
  reminderFolderToResponse,
  reminderFoldersToResponses,

  -- Model/Internal
  getRemindersM,
  insertRemindersM,
  getReminderM,
  updateReminderM,
  deleteReminderM,
  getReminderFoldersM,
  insertReminderFoldersM,
  getReminderFolderM,
  insertReminderFolderM,
  updateReminderFolderM,
  deleteReminderFolderM
) where



import           All.Prelude
import           LN.Lib.Url (toPrettyUrl)
import           LN.T
import           Import
import           Misc.Codec (keyToInt64)



--
-- Handler
--

getRemindersR :: Handler Value
getRemindersR = run $ do
  user_id <- _requireAuthId

  sp <- lookupStandardParams

  (toJSON . remindersToResponses) <$> getRemindersM user_id (spReminderFolderId sp)



postRemindersR :: Handler Value
postRemindersR = run $ do

  user_id <- _requireAuthId

  sp <- lookupStandardParams

  reminder_request <- requireJsonBody :: HandlerEff ReminderRequest
  (toJSON . reminderToResponse) <$> insertRemindersM user_id (spReminderFolderId sp) reminder_request



getReminderR :: ReminderId -> Handler Value
getReminderR reminder_id = run $ do
  user_id <- _requireAuthId
  (toJSON . reminderToResponse) <$> getReminderM user_id reminder_id



putReminderR :: ReminderId -> Handler Value
putReminderR reminder_id = run $ do
  user_id <- _requireAuthId
  reminder_request <- requireJsonBody
  (toJSON . reminderToResponse) <$> updateReminderM user_id reminder_id reminder_request



deleteReminderR :: ReminderId -> Handler Value
deleteReminderR reminder_id = run $ do
  user_id <- _requireAuthId
  void $ deleteReminderM user_id reminder_id
  pure $ toJSON ()






-- Folders
--

getReminderFoldersR :: Handler Value
getReminderFoldersR = run $ do
  user_id <- _requireAuthId

  sp <- lookupStandardParams

  (toJSON . reminderFoldersToResponses) <$> getReminderFoldersM user_id (spReminderFolderId sp)



postReminderFoldersR :: Handler Value
postReminderFoldersR = run $ do
  user_id <- _requireAuthId
  sp <- lookupStandardParams
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> insertReminderFoldersM user_id (spReminderFolderId sp) reminder_folder_request



getReminderFolderR :: ReminderFolderId -> Handler Value
getReminderFolderR reminder_folder_id = run $ do
  user_id <- _requireAuthId
  (toJSON . reminderFolderToResponse) <$> getReminderFolderM user_id reminder_folder_id



postReminderFolderR :: ReminderFolderId -> Handler Value
postReminderFolderR reminder_folder_id = run $ do
  user_id <- _requireAuthId
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> insertReminderFolderM user_id reminder_folder_id reminder_folder_request



putReminderFolderR :: ReminderFolderId -> Handler Value
putReminderFolderR reminder_folder_id = run $ do
  user_id <- _requireAuthId
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> updateReminderFolderM user_id reminder_folder_id reminder_folder_request



deleteReminderFolderR :: ReminderFolderId -> Handler Value
deleteReminderFolderR reminder_folder_id = run $ do
  user_id <- _requireAuthId
  void $ deleteReminderFolderM user_id reminder_folder_id
  pure $ toJSON ()







--
-- Model/Function
--

reminderRequestToReminder :: UserId -> ReminderFolderId -> ReminderRequest -> Reminder
reminderRequestToReminder user_id reminder_folder_id ReminderRequest{..} = Reminder {
  reminderUserId         = user_id,
  reminderParentFolderId = reminder_folder_id,
  reminderData           = reminderRequestData,
  reminderActive         = True,
  reminderGuard          = reminderRequestGuard,
  reminderCreatedAt      = Nothing,
  reminderModifiedAt     = Nothing,
  reminderActivityAt     = Nothing
}



reminderToResponse :: Entity Reminder -> ReminderResponse
reminderToResponse (Entity reminder_id Reminder{..}) = ReminderResponse {
  reminderResponseId             = keyToInt64 reminder_id,
  reminderResponseUserId         = keyToInt64 reminderUserId,
  reminderResponseParentFolderId = keyToInt64 reminderParentFolderId,
  reminderResponseData           = reminderData,
  reminderResponseActive         = reminderActive,
  reminderResponseGuard          = reminderGuard,
  reminderResponseCreatedAt      = reminderCreatedAt,
  reminderResponseModifiedAt     = reminderModifiedAt,
  reminderResponseActivityAt     = reminderActivityAt
}



remindersToResponses :: [Entity Reminder] -> ReminderResponses
remindersToResponses reminders = ReminderResponses {
  reminderResponses = map reminderToResponse reminders
}



reminderFolderRequestToReminderFolder :: UserId -> Maybe ReminderFolderId -> ReminderFolderRequest -> ReminderFolder
reminderFolderRequestToReminderFolder user_id reminder_folder_id ReminderFolderRequest{..} = ReminderFolder {
  reminderFolderUserId      = user_id,
  reminderFolderParentId    = reminder_folder_id,
  reminderFolderName        = toPrettyUrl reminderFolderRequestDisplayName,
  reminderFolderDisplayName = reminderFolderRequestDisplayName,
  reminderFolderDescription = reminderFolderRequestDescription,
  reminderFolderVisibility  = reminderFolderRequestVisibility,
  reminderFolderActive      = True,
  reminderFolderGuard       = reminderFolderRequestGuard,
  reminderFolderCreatedAt   = Nothing,
  reminderFolderModifiedAt  = Nothing,
  reminderFolderActivityAt  = Nothing
}



reminderFolderToResponse :: Entity ReminderFolder -> ReminderFolderResponse
reminderFolderToResponse (Entity reminderFolder_id ReminderFolder{..}) = ReminderFolderResponse {
  reminderFolderResponseId             = keyToInt64 reminderFolder_id,
  reminderFolderResponseUserId         = keyToInt64 reminderFolderUserId,
  reminderFolderResponseParentFolderId = fmap keyToInt64 reminderFolderParentId,
  reminderFolderResponseName           = reminderFolderName,
  reminderFolderResponseDisplayName    = reminderFolderDisplayName,
  reminderFolderResponseDescription    = reminderFolderDescription,
  reminderFolderResponseVisibility     = reminderFolderVisibility,
  reminderFolderResponseActive         = reminderFolderActive,
  reminderFolderResponseGuard          = reminderFolderGuard,
  reminderFolderResponseCreatedAt      = reminderFolderCreatedAt,
  reminderFolderResponseModifiedAt     = reminderFolderModifiedAt,
  reminderFolderResponseActivityAt     = reminderFolderActivityAt

}



reminderFoldersToResponses :: [Entity ReminderFolder] -> ReminderFolderResponses
reminderFoldersToResponses reminderFolders = ReminderFolderResponses {
  reminderFolderResponses = map reminderFolderToResponse reminderFolders
}







--
-- Model/Internal
--

getRemindersM :: UserId -> Maybe ReminderFolderId -> HandlerEff [Entity Reminder]
getRemindersM user_id mfolder_id = do
  case mfolder_id of
    Nothing -> do
      selectListDb' [ ReminderUserId ==. user_id ] [] ReminderId
    Just folder_id -> do
      selectListDb' [ ReminderUserId ==. user_id, ReminderParentFolderId ==. folder_id ] [] ReminderId



insertRemindersM :: UserId -> Maybe ReminderFolderId -> ReminderRequest -> HandlerEff (Entity Reminder)
insertRemindersM user_id mfolder_id reminder_request = do
  case mfolder_id of
    Nothing -> notFound
    Just folder_id -> do

      ts <- timestampH'

      let reminder = (reminderRequestToReminder user_id folder_id reminder_request) { reminderCreatedAt = Just ts }
      insertEntityDb reminder



getReminderM :: UserId -> ReminderId -> HandlerEff (Entity Reminder)
getReminderM _ reminder_id = do
  notFoundMaybe =<< selectFirstDb [ ReminderId ==. reminder_id ] []



-- | We cant update the ReminderFolderId here..
--
updateReminderM :: UserId -> ReminderId -> ReminderRequest -> HandlerEff (Entity Reminder)
updateReminderM user_id reminder_id reminder_request = do

  ts <- timestampH'

  let
    Reminder{..} = (reminderRequestToReminder user_id (int64ToKey' 0) reminder_request) { reminderModifiedAt = Just ts }

  updateWhereDb
    [ ReminderUserId ==. user_id, ReminderId ==. reminder_id ]
    [ ReminderModifiedAt =. reminderModifiedAt
    , ReminderData =. reminderData
    ]

  notFoundMaybe =<< selectFirstDb [ ReminderUserId ==. user_id, ReminderId ==. reminder_id ] []



deleteReminderM :: UserId -> ReminderId -> HandlerEff ()
deleteReminderM user_id reminder_id = do
  deleteWhereDb [ ReminderUserId ==. user_id, ReminderId ==. reminder_id ]




-- | Get the CHILDREN folders
--
getReminderFoldersM :: UserId -> Maybe ReminderFolderId -> HandlerEff [Entity ReminderFolder]
getReminderFoldersM user_id mfolder_id = do
  selectListDb' [ ReminderFolderUserId ==. user_id, ReminderFolderParentId ==. mfolder_id ] [] ReminderFolderId



insertReminderFoldersM :: UserId -> Maybe ReminderFolderId -> ReminderFolderRequest -> HandlerEff (Entity ReminderFolder)
insertReminderFoldersM user_id mfolder_id reminder_folder_request = do

  ts <- timestampH'

  let
    reminder_folder = (reminderFolderRequestToReminderFolder user_id mfolder_id reminder_folder_request) { reminderFolderCreatedAt = Just ts }

  insertEntityDb reminder_folder



getReminderFolderM :: UserId -> ReminderFolderId -> HandlerEff (Entity ReminderFolder)
getReminderFolderM _ reminder_folder_id = do
  notFoundMaybe =<< selectFirstDb [ ReminderFolderId ==. reminder_folder_id ] []



insertReminderFolderM :: UserId -> ReminderFolderId -> ReminderFolderRequest -> HandlerEff (Entity ReminderFolder)
insertReminderFolderM user_id reminder_folder_id reminder_folder_request = do

  ts <- timestampH'

  let
    reminder_folder = (reminderFolderRequestToReminderFolder user_id (Just reminder_folder_id) reminder_folder_request) { reminderFolderCreatedAt = Just ts }

  insertEntityDb reminder_folder



-- | Can't update ReminderFolderParentId here..
--
updateReminderFolderM :: UserId -> ReminderFolderId -> ReminderFolderRequest -> HandlerEff (Entity ReminderFolder)
updateReminderFolderM user_id reminder_folder_id reminder_folder_request = do

  ts <- timestampH'

  let
    ReminderFolder{..} = (reminderFolderRequestToReminderFolder user_id Nothing reminder_folder_request) { reminderFolderModifiedAt = Just ts }

  updateWhereDb
    [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ]
    [ ReminderFolderModifiedAt =. reminderFolderModifiedAt
    ]

  notFoundMaybe =<< selectFirstDb [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ] []



deleteReminderFolderM :: UserId -> ReminderFolderId -> HandlerEff ()
deleteReminderFolderM user_id reminder_folder_id = do
  deleteCascadeWhereDb [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ]
