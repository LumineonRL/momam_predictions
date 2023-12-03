
userid_pie <- GET("https://api.twitch.tv/helix/users",
add_headers(`Client-ID`=client_id,
            Authorization=str_c("Bearer ",access_token)),
query=list(login="iateyourpie")) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  use_series("data") %>%
  use_series("id") %>%
  unlist()

userid_spike <- GET("https://api.twitch.tv/helix/users",
                    add_headers(`Client-ID`=client_id,
                                Authorization=str_c("Bearer ",access_token)),
                    query=list(login="spikevegeta")) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  use_series("data") %>%
  use_series("id") %>%
  unlist()

userids <- tibble(id=userids,user=c("iateyourpie","spikevegeta"))

dbWriteTable(db,"channels_userids",userids,overwrite=T)