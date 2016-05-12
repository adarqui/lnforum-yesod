{-# LANGUAGE RecordWildCards #-}

module Model.Reminder.Function (
  reminderRequestToReminder,
  reminderToResponse,
  remindersToResponses,
  reminderFolderRequestToReminderFolder,
  reminderFolderToResponse,
  reminderFoldersToResponses,
) where



import           LN.T
import           Import
import           Misc.Codec (keyToInt64)



reminderRequestToReminder :: UserId -> ReminderFolderId -> ReminderRequest -> Reminder
reminderRequestToReminder user_id reminder_folder_id ReminderRequest{..} = Reminder {
  reminderUserId         = user_id,
  reminderParentFolderId = reminder_folder_id,
  reminderData           = reminderRequestData,
  reminderActive         = True,
  reminderCreatedAt      = Nothing,
  reminderModifiedAt     = Nothing
}



reminderToResponse :: Entity Reminder -> ReminderResponse
reminderToResponse (Entity reminder_id Reminder{..}) = ReminderResponse {
  reminderResponseId = keyToInt64 reminder_id,
  reminderResponseUserId = keyToInt64 reminderUserId,
  reminderResponseParentFolderId = keyToInt64 reminderParentFolderId,
  reminderResponseData = reminderData,
  reminderResponseCreatedAt = reminderCreatedAt,
  reminderResponseModifiedAt = reminderModifiedAt
}



remindersToResponses :: [Entity Reminder] -> ReminderResponses
remindersToResponses reminders = ReminderResponses {
  reminderResponses = map reminderToResponse reminders
}



reminderFolderRequestToReminderFolder :: UserId -> Maybe ReminderFolderId -> ReminderFolderRequest -> ReminderFolder
reminderFolderRequestToReminderFolder user_id reminder_folder_id ReminderFolderRequest{..} = ReminderFolder {
  reminderFolderUserId      = user_id,
  reminderFolderParentId    = reminder_folder_id,
  reminderFolderName        = reminderFolderRequestName,
  reminderFolderDescription = reminderFolderRequestDescription,
  reminderFolderActive      = True,
  reminderFolderCreatedAt   = Nothing,
  reminderFolderModifiedAt  = Nothing
}



reminderFolderToResponse :: Entity ReminderFolder -> ReminderFolderResponse
reminderFolderToResponse (Entity reminderFolder_id ReminderFolder{..}) = ReminderFolderResponse {
  reminderFolderResponseId = keyToInt64 reminderFolder_id,
  reminderFolderResponseUserId = keyToInt64 reminderFolderUserId,
  reminderFolderResponseParentFolderId = fmap keyToInt64 reminderFolderParentId,
  reminderFolderResponseName = reminderFolderName,
  reminderFolderResponseDescription = reminderFolderDescription,
  -- TODO FIXME
  reminderFolderResponseVisibility = Public, -- reminderFolderVisibility,
  reminderFolderResponseCreatedAt = reminderFolderCreatedAt,
  reminderFolderResponseModifiedAt = reminderFolderModifiedAt
}



reminderFoldersToResponses :: [Entity ReminderFolder] -> ReminderFolderResponses
reminderFoldersToResponses reminderFolders = ReminderFolderResponses {
  reminderFolderResponses = map reminderFolderToResponse reminderFolders
}
