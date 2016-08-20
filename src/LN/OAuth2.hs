-- Mostly stolen from Thoughtbot's Carnival
--

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LN.OAuth2 (
  User(..),
  authenticateUser
) where



import           LN.All.User.Shared      (insertUsers_TasksM)
import           LN.Import.NoFoundation
import           LN.Misc.Codec
import           LN.Sanitize.Internal    (toSafeName)
import           LN.T.Profile            (ProfileX (..))
import           Yesod.Auth.GoogleEmail2



-- data Creds
--
-- User credentials
--
-- credsPlugin :: Text -- How the user was authenticated
-- credsIdent :: Text -- Identifier. Exact meaning depends on plugin.
-- credsExtra :: [(Text, Text)]
--
authenticateUser :: AuthId m ~ UserId => Creds m -> DB (AuthenticationResult m)
authenticateUser creds@Creds{..} = do

  -- mapM_ (updateByEmail credsPlugin)
  --   $ fmap profileEmail
  --   $ extraToProfileX credsPlugin credsExtra

  case profileX of
    Left err           -> pure $ ServerError err
    Right ProfileX{..} -> do
      m_user <- getBy $ UniqueEmail profileEmail

      now <- liftIO $ getCurrentTime

      let
        e_user    = credsToUser now creds
        m_user_id = entityKey <$> m_user

      case m_user_id of
        Nothing      -> authNew e_user
        Just user_id -> authExisting e_user user_id

  where

  profileX     = extraToProfileX credsPlugin credsExtra

  updateByEmail "github" email = updateWhere
    [ UserEmail        ==. email ]
    [ UserPlugin       =. credsPlugin
    , UserGithubIdent  =. Just credsIdent
    ]

  updateByEmail "googleemail2" email = updateWhere
    [ UserEmail        ==. email ]
    [ UserPlugin       =. credsPlugin
    , UserGoogleIdent  =. Just credsIdent
    ]

  updateByEmail _ _ = pure ()



  authNew (Left err)   = pure $ ServerError $ credsPlugin <> ": " <> err
  authNew (Right user) = do

    -- Add user, then queue up a CreateUserProfile background job
    --
    returned_user <- insertEntity user
    void $ liftIO $ insertUsers_TasksM returned_user
    pure $ Authenticated (entityKey returned_user)



  authExisting e_user userId = do
    -- TODO FIXME: replacing a user due to oauth2 changes..
    --
    -- mapM_ (replace userId) e_user
    case e_user of
      Left err   -> pure $ ServerError err
      Right user -> do
        updateByEmail credsPlugin (userEmail user)
        pure $ Authenticated userId



credsToUser :: UTCTime -> Creds m -> Either Text User
credsToUser now Creds{..} = User
  <$> ((toSafeName . profileName) <$> e_profile) -- ^ name
  <*> (profileName <$> e_profile)                -- ^ display name
  <*> (profileName <$> e_profile)                -- ^ full name
  <*> (profileEmail <$> e_profile)               -- ^ email
  <*> pure email_md5                             -- ^ email md5
  <*> pure credsPlugin                           -- ^ plugin: github, google, etc..
  <*> pure github_ident                          -- ^ github ident
  <*> pure github_ts                             -- ^ github created at
  <*> pure google_ident                          -- ^ google ident
  <*> pure google_ts                             -- ^ google created at
  <*> pure Nothing                               -- ^ accept TOS
  <*> pure True                                  -- ^ active
  <*> pure 0                                     -- ^ guard
  <*> pure (Just now)                            -- ^ created at
  <*> pure Nothing                               -- ^ modified at
  <*> pure Nothing                               -- ^ activity at
  where
  e_profile = extraToProfileX credsPlugin credsExtra
  email_md5 = case e_profile of
                Left _ -> "md5"
                Right ProfileX{..} -> md5Text profileEmail

  github_ident = if credsPlugin == "github"
                    then Just credsIdent
                    else Nothing
  github_ts    = maybe Nothing (const $ Just now) github_ident

  google_ident = if credsPlugin == "googleemail2"
                    then Just credsIdent
                    else Nothing
  google_ts    = maybe Nothing (const $ Just now) google_ident



extraToProfileX :: Text -> [(Text, Text)] -> Either Text ProfileX
extraToProfileX "dummy" extra        = githubProfileX extra
extraToProfileX "github" extra       = githubProfileX extra
extraToProfileX "googleemail2" extra = googleProfileX extra
extraToProfileX plugin _             = Left $ "Invalid plugin: " <> plugin



githubProfileX :: [(Text, Text)] -> Either Text ProfileX
githubProfileX extra = ProfileX
  <$> lookupExtra "login" extra
  <*> lookupExtra "name" extra
  <*> lookupExtra "email" extra



googleProfileX :: [(Text, Text)] -> Either Text ProfileX
googleProfileX extra = ProfileX
  <$> (handleName =<< decodeEitherText =<< lookupExtra "name" extra)
  <*> (handleName =<< decodeEitherText =<< lookupExtra "name" extra)
  <*> (handleEmails =<< decodeEitherText =<< lookupExtra "emails" extra)

  where
    handleName :: Name -> Either Text Text
    handleName Name{..} =
        case (nameFormatted, nameGiven, nameFamily) of
            (Just formatted, _, _) -> Right $ formatted
            (_, Just given, Just family) -> Right $ given <> " " <> family
            (_, Just given, _) -> Right given
            (_, _, Just family) -> Right family
            _ -> Left "user has no name"

    handleEmails :: [Email] -> Either Text Text
    handleEmails []        = Left "user has no emails"
    handleEmails (email:_) = Right $ emailValue email




lookupExtra :: Text -> [(Text, b)] -> Either Text b
lookupExtra k extra =
  maybe (Left $ "missing key " <> k) Right $ lookup k extra
