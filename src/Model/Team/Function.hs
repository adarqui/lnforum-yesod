{-# LANGUAGE RecordWildCards #-}

module Model.Team.Function (
  teamRequestToTeam,
  teamToResponse,
  teamsToResponses,
) where



import Model.Prelude



teamRequestToTeam :: UserId -> OrganizationId -> TeamRequest -> Team
teamRequestToTeam user_id org_id TeamRequest{..} = Team {
  teamUserId      = user_id,
  teamOrgId       = org_id,
  teamName        = teamRequestName,
  teamDescription = teamRequestDescription,
  teamMembership  = teamRequestMembership,
  teamIcon        = teamRequestIcon,
  teamTags        = teamRequestTags,
  teamVisibility  = teamRequestVisibility,
  teamActive      = True,
  teamCreatedAt   = Nothing,
  teamModifiedBy  = Nothing,
  teamModifiedAt  = Nothing
}



teamToResponse :: Entity Team -> TeamResponse
teamToResponse (Entity team_id Team{..}) = TeamResponse {
  teamResponseId          = keyToInt64 team_id,
  teamResponseUserId      = keyToInt64 teamUserId,
  teamResponseOrgId       = keyToInt64 teamOrgId,
  teamResponseName        = teamName,
  teamResponseDescription = teamDescription,
  teamResponseMembership  = teamMembership,
  teamResponseIcon        = teamIcon,
  teamResponseTags        = teamTags,
  teamResponseVisibility  = teamVisibility,
  teamResponseCreatedAt   = teamCreatedAt,
  teamResponseModifiedBy  = fmap keyToInt64 teamModifiedBy,
  teamResponseModifiedAt  = teamModifiedAt
}



teamsToResponses :: [Entity Team] -> TeamResponses
teamsToResponses teams = TeamResponses {
  teamResponses = map teamToResponse teams
}
