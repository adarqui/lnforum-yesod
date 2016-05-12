{-# LANGUAGE RecordWildCards #-}

module Model.Reminder.Internal (
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



import           Model.Prelude
import           Model.Reminder.Function



getRemindersM :: UserId -> Maybe ReminderFolderId -> Handler [Entity Reminder]
getRemindersM user_id mfolder_id = do
  case mfolder_id of
    Nothing -> do
      selectListDb' [ ReminderUserId ==. user_id ] [] ReminderId
    Just folder_id -> do
      selectListDb' [ ReminderUserId ==. user_id, ReminderParentFolderId ==. folder_id ] [] ReminderId



insertRemindersM :: UserId -> Maybe ReminderFolderId -> ReminderRequest -> Handler (Entity Reminder)
insertRemindersM user_id mfolder_id reminder_request = do
  case mfolder_id of
    Nothing -> notFound
    Just folder_id -> do

      ts <- timestampH'

      let reminder = (reminderRequestToReminder user_id folder_id reminder_request) { reminderCreatedAt = Just ts }
      insertEntityDb reminder



getReminderM :: UserId -> ReminderId -> Handler (Entity Reminder)
getReminderM _ reminder_id = do
  notFoundMaybe =<< selectFirstDb [ ReminderId ==. reminder_id ] []



-- | We cant update the ReminderFolderId here..
--
updateReminderM :: UserId -> ReminderId -> ReminderRequest -> Handler (Entity Reminder)
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



deleteReminderM :: UserId -> ReminderId -> Handler ()
deleteReminderM user_id reminder_id = do
  deleteWhereDb [ ReminderUserId ==. user_id, ReminderId ==. reminder_id ]




-- | Get the CHILDREN folders
--
getReminderFoldersM :: UserId -> Maybe ReminderFolderId -> Handler [Entity ReminderFolder]
getReminderFoldersM user_id mfolder_id = do
  selectListDb' [ ReminderFolderUserId ==. user_id, ReminderFolderParentId ==. mfolder_id ] [] ReminderFolderId



insertReminderFoldersM :: UserId -> Maybe ReminderFolderId -> ReminderFolderRequest -> Handler (Entity ReminderFolder)
insertReminderFoldersM user_id mfolder_id reminder_folder_request = do

  ts <- timestampH'

  let
    reminder_folder = (reminderFolderRequestToReminderFolder user_id mfolder_id reminder_folder_request) { reminderFolderCreatedAt = Just ts }

  insertEntityDb reminder_folder



getReminderFolderM :: UserId -> ReminderFolderId -> Handler (Entity ReminderFolder)
getReminderFolderM _ reminder_folder_id = do
  notFoundMaybe =<< selectFirstDb [ ReminderFolderId ==. reminder_folder_id ] []



insertReminderFolderM :: UserId -> ReminderFolderId -> ReminderFolderRequest -> Handler (Entity ReminderFolder)
insertReminderFolderM user_id reminder_folder_id reminder_folder_request = do

  ts <- timestampH'

  let
    reminder_folder = (reminderFolderRequestToReminderFolder user_id (Just reminder_folder_id) reminder_folder_request) { reminderFolderCreatedAt = Just ts }

  insertEntityDb reminder_folder



-- | Can't update ReminderFolderParentId here..
--
updateReminderFolderM :: UserId -> ReminderFolderId -> ReminderFolderRequest -> Handler (Entity ReminderFolder)
updateReminderFolderM user_id reminder_folder_id reminder_folder_request = do

  ts <- timestampH'

  let
    ReminderFolder{..} = (reminderFolderRequestToReminderFolder user_id Nothing reminder_folder_request) { reminderFolderModifiedAt = Just ts }

  updateWhereDb
    [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ]
    [ ReminderFolderModifiedAt =. reminderFolderModifiedAt
    ]

  notFoundMaybe =<< selectFirstDb [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ] []



deleteReminderFolderM :: UserId -> ReminderFolderId -> Handler ()
deleteReminderFolderM user_id reminder_folder_id = do
  deleteCascadeWhereDb [ ReminderFolderUserId ==. user_id, ReminderFolderId ==. reminder_folder_id ]
