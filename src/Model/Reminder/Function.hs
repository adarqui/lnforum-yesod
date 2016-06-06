{-# LANGUAGE RecordWildCards #-}

module Model.Reminder.Function (
  reminderRequestToReminder,
  reminderToResponse,
  remindersToResponses,
  reminderFolderRequestToReminderFolder,
  reminderFolderToResponse,
  reminderFoldersToResponses,
) where



import           LN.Lib.Url (prettyName)
import           LN.T
import           Import
import           Misc.Codec (keyToInt64)



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
  reminderFolderName        = prettyName reminderFolderRequestDisplayName,
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
