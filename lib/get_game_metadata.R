# Takes a list of game id numbers and returns a query that will search them for relevant metadata.
get_query <- function(game_list) {
  game_list %>%
    toString() %>%
    str_c('fields id, genres, platforms, player_perspectives, themes, total_rating, franchises, involved_companies, first_release_date; where id = (', ., '); limit 500;') %>%
    return()
}

# Posts the query returned from get_query in the vgdb API and returns the results as a tibble.
post_query <- function(query) {
  POST(str_c(api_url, end_games),
       add_headers(`Client-ID` = client_id,
                   Authorization = str_c("Bearer ", access_token)),
       body = query) %>%
    use_series("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    as_tibble() %>%
    return()
}

# Unpivots the data into 3 simple columns and unnests list data. For example, the data for one game may come
# in as a list in the form of "c(1, 2, 3, 4)". This will properly split that list into a row for each value in the list.
process_query <- function(query_result) {
  query_result %>%
    mutate(first_release_date = as.list(year(as_datetime(first_release_date))),
           total_rating = as.list(as.integer(total_rating))) %>%
    pivot_longer(!id) %>% 
    unnest(cols = c(value)) %>%
    return()
}

# Merges the data into the MOMAM database.
save_results <- function(db, processed_result) {
  insert <- dbSendStatement(db, "INSERT INTO games_metadata (id, name, value) VALUES (?, ?, ?) ON CONFLICT DO NOTHING")
  dbBind(insert, list(as.vector(processed_result$id), as.vector(processed_result$name), as.vector(processed_result$value)))
  dbClearResult(insert)
}

# Higher level function that executes the above helper functions, while also
# ensuring each API call stays within the API's restrictions.
games_run_query <- function(db) {
  games_all <- dbGetQuery(db, "SELECT DISTINCT game_id FROM game_data")
  
  games_count <- nrow(games_all)
  QUERY_LIMIT <- 500L  #The API only supports 500 items in a single request. Since pie and spike have played more than
  #500 games, the metadata needs to be grabbed in multiple queries.
  split_num <- 1L
  if(games_count > QUERY_LIMIT) {
    max_splits <- ceiling(games_count / QUERY_LIMIT)
    
    while(split_num < max_splits) {
      games_current <- rep(NA, QUERY_LIMIT)
      games_current <- games_all$game_id[seq((split_num-1)*QUERY_LIMIT+1, split_num * QUERY_LIMIT)]
      
      get_query(games_current) %>%
        post_query() %>%
        process_query() %>%
        save_results(db, .)
      
      split_num <- split_num + 1
    }
  }
  games_current <- ifelse(games_count %% QUERY_LIMIT == 0, 
                          rep(NA, QUERY_LIMIT),
                          rep(NA, games_count %% QUERY_LIMIT))
  games_current <- games_all$game_id[seq((split_num-1)*QUERY_LIMIT+1, games_count)]
  
  get_query(games_current) %>%
    post_query() %>%
    process_query() %>%
    save_results(db, .)
}