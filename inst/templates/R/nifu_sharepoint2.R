nifu_sharepoint_share_reports <-
  function(x, path, team_name = "21209-91 NFR-ADVISE ON LIES") {
    scopes <- c(
      "https://graph.microsoft.com/Files.ReadWrite.All",
      "https://graph.microsoft.com/User.Read",
      "https://graph.microsoft.com/Team.ReadBasic.All",
      "https://graph.microsoft.com/Mail.Read",
      "https://graph.microsoft.com/Group.Read.All",
      "openid", "offline_access"
    )
    app <- "d44a05d5-c6a5-4bbb-82d2-443123722380" # for local use only
    token <- AzureAuth::get_azure_token(resource = scopes, token_args = "NIFU", tenant = app, version=2)
    sp_aol <- Microsoft365R::get_team(team_name = team_name, token = token)
    sp_aol_drive <- sp_aol$get_drive()
    sp_aol_drive$create_share_link(itemid = "013DVKBYG7B32Y7YSI3JAICXTKA6YY5OY6")

  }
