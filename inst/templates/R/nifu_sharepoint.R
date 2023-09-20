sp_scopes <- c(
  "https://graph.microsoft.com/Files.ReadWrite.All",
  "https://graph.microsoft.com/User.Read",
  "https://graph.microsoft.com/Team.ReadBasic.All",
  "https://graph.microsoft.com/Mail.Read",
  # "https://graph.microsoft.com/Channel.ReadBasic.All",
  # "https://graph.microsoft.com/Directory.Read.All",
  # "https://graph.microsoft.com/Directory.ReadWrite.All",
  "openid", "offline_access"
)
sp_app <- "d44a05d5-c6a5-4bbb-82d2-443123722380" # for local use only
sp_token <- AzureAuth::get_azure_token(resource = sp_scopes, tenant = "NIFU", app = sp_app, version=2)
sp_aol <- Microsoft365R::get_team(team_name = "21209-91 NFR-ADVISE ON LIES", token=token)
sp_aol_drive <- sp_aol$get_drive()
# sp_aol$create_channel(channel_name = "Rapporter for deling")
sp_folder <- sp_aol_drive$list_files(path = "Rapporter for deling/Kun for Ingvar")
sp_file <- sp_aol_drive$get_item(path = "Rapporter for deling/Kun for Ingvar/Bø!.docx")
sp_file_link <- my_item$create_share_link(type = "view", expiry = NULL, scope="anonymous")

Microsoft365R::list_chats()
sp_team1 <- Microsoft365R::get_chat(chat_id = , tenant="NIFU", app = sp_app, token = token, scopes = sp_scopes)