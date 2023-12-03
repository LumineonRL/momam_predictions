get_client_id <- function(){
  return("YOUR CLIENT ID HERE")
}

get_client_secret <- function(){
  return("YOUR CLIENT SECRET HERE")
}

get_access_token <- function(){
  POST("https://id.twitch.tv/oauth2/token",
       query = list(client_id=get_client_id(),
                    client_secret=get_client_secret(),
                    grant_type="client_credentials")) %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    use_series("access_token") %>%
    return()
}
