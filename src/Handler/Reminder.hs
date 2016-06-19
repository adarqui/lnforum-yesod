module Handler.Reminder (
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
  deleteReminderFolderR
) where



import           Handler.Prelude
import           Model.Reminder



getRemindersR :: Handler Value
getRemindersR = do
  user_id <- requireAuthId

  sp <- lookupStandardParams

  (toJSON . remindersToResponses) <$> getRemindersM user_id (spReminderFolderId sp)



postRemindersR :: Handler Value
postRemindersR = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  reminder_request <- requireJsonBody :: Handler ReminderRequest
  (toJSON . reminderToResponse) <$> insertRemindersM user_id (spReminderFolderId sp) reminder_request



getReminderR :: ReminderId -> Handler Value
getReminderR reminder_id = do
  user_id <- requireAuthId
  (toJSON . reminderToResponse) <$> getReminderM user_id reminder_id



putReminderR :: ReminderId -> Handler Value
putReminderR reminder_id = do
  user_id <- requireAuthId
  reminder_request <- requireJsonBody
  (toJSON . reminderToResponse) <$> updateReminderM user_id reminder_id reminder_request



deleteReminderR :: ReminderId -> Handler Value
deleteReminderR reminder_id = do
  user_id <- requireAuthId
  void $ deleteReminderM user_id reminder_id
  pure $ toJSON ()






-- Folders
--

getReminderFoldersR :: Handler Value
getReminderFoldersR = do
  user_id <- requireAuthId

  sp <- lookupStandardParams

  (toJSON . reminderFoldersToResponses) <$> getReminderFoldersM user_id (spReminderFolderId sp)



postReminderFoldersR :: Handler Value
postReminderFoldersR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> insertReminderFoldersM user_id (spReminderFolderId sp) reminder_folder_request



getReminderFolderR :: ReminderFolderId -> Handler Value
getReminderFolderR reminder_folder_id = do
  user_id <- requireAuthId
  (toJSON . reminderFolderToResponse) <$> getReminderFolderM user_id reminder_folder_id



postReminderFolderR :: ReminderFolderId -> Handler Value
postReminderFolderR reminder_folder_id = do
  user_id <- requireAuthId
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> insertReminderFolderM user_id reminder_folder_id reminder_folder_request



putReminderFolderR :: ReminderFolderId -> Handler Value
putReminderFolderR reminder_folder_id = do
  user_id <- requireAuthId
  reminder_folder_request <- requireJsonBody
  (toJSON . reminderFolderToResponse) <$> updateReminderFolderM user_id reminder_folder_id reminder_folder_request



deleteReminderFolderR :: ReminderFolderId -> Handler Value
deleteReminderFolderR reminder_folder_id = do
  user_id <- requireAuthId
  void $ deleteReminderFolderM user_id reminder_folder_id
  pure $ toJSON ()
