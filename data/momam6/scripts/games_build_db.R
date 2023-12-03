#Not in use.

access_token <- get_access_token()

api_url <- "https://api.igdb.com/v4"
end_games <- "/games"
end_channels <- "/channels"
end_categories <- "/categories"
end_genres <- "/genres"
end_age <- "/age_ratings"
end_company <- "/involved_companies"
end_engines <- "/game_engines"
end_platforms <- "/platforms"
end_franchises <- "/franchises"
end_themes <- "/themes"

get_genres <- function(client_id,access_token){
  POST(str_c(api_url,end_genres),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 100;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()
}
get_platforms <- function(client_id,access_token){
  POST(str_c(api_url,end_platforms),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 200;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()  
}
get_themes <- function(client_id,access_token){
  POST(str_c(api_url,end_themes),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body="fields id,name; limit 100;") %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    arrange(id) %>%
    return()
}
genres <- get_genres(client_id,access_token)
platforms <- get_platforms(client_id,access_token)
themes <- get_themes(client_id,access_token)

