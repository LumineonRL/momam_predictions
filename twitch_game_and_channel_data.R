# Takes csv files generated from sullygnome containing information on what games
# and how long each game was played on pie and spike's channels each year as input.
# Uses those files to query the vgdb API and retrieve metadata for each of those games.

library(stringr)
library(dplyr)
library(magrittr)
library(readr)
library(httr)
library(jsonlite)
library(RSQLite)
library(tidyr)
library(data.table)
library(purrr)
library(stringi)
library(stringr)
library(lubridate)

db <- dbConnect(SQLite(),dbname="MOMAM.sqlite")

csvs <- str_c("channel_data/", list.files("channel_data"))
tablenames <- str_c(tolower(str_extract(csvs, "(pie|Spike)")),  str_extract(csvs, "20[:number:]+"))


#End points with necessary information from the API.
api_url <- "https://api.igdb.com/v4"
end_games <- "/games"
end_categories <- "/categories"
end_genres <- "/genres"
end_age <- "/age_ratings"
end_company <- "/involved_companies"
end_engines <- "/game_engines"
end_platforms <- "/platforms"
end_franchises <- "/franchises"
end_themes <- "/themes"
end_keywords <- "/keywords"
end_perspectives <- "/player_perspectives"

client_id <- get_client_id()
access_token <- get_access_token()

#dbname should be a database.
#tablename is a string
#csv is a string leading to a csv file from sullygnome.




#This is unfortunately EXTREMELY ugly since the way the URLs handle "'s" is
#inconsistent. It's pretty much a 50/50 whether "Mario's" will turn into
#"marios" or "mario-s" in the URL. &s and +s are also a nightmare.
#This could absolutely be cleaned up a bit but that's a non-trivial matter
#and a project for another day.
construct_yearly_time <- function(dbname, csv){
  message(c("Now reading:", csv))
  
  v_streamer <- tolower(str_extract(csv, "(pie|Spike)"))
  v_year <- str_extract(csv, "20[:number:]+") %>%
    as.numeric()
  
  yearly_data <- read_csv(csv) %>%
    select(2, 3) %>%
    `colnames<-` (c("game","minutes")) %>%
    mutate(streamer = v_streamer,
           year = v_year,
           game = str_extract(game, "[^|]+"),
           game = str_replace(game, "é", "e"),
           game = sapply(game, pokemon_checker),
           url = str_to_lower(game),
           url = str_replace_all(url,"&","and"),
           url = str_replace_all(url,"\\+","plus"),
           url = str_remove_all(url,"[^([:alnum:]-?|[:space:]|\'|$)]"),
           url = str_replace_all(url,"([:space:]+|\')","-"),
           url = str_replace(url,"chip-s","chips"),
           url = str_replace(url,"freddy-s","freddys"),
           url = str_replace(url,"igi-s-man","igis-man"),
           url = str_replace(url,"known-s-bat","knowns-bat"),
           url = str_replace(url,"let-s-go-pika","lets-go-pika"),
           url = str_replace(url,"pooh-s-home","poohs-home"),
           url = str_replace(url,"ter-s-aren","ters-aren"),
           url = str_replace(url,"shi-s-wool","shis-wool"),
           url = str_replace(url,"super-mario-3d-world-plus-bowser-s-fury","super-mario-3d-world-plus-bowsers-fury"),
           url = str_replace(url,"shi-s-saf","shis-saf"),
           url = str_replace(url,"mafiagg","mafia-dot-gg"),
           url = str_replace(url,"o-hare","ohare"),
           url = str_replace(url,"sid-meier-s-civilization-vi","sid-meiers-civilization-vi"),
           url = str_replace(url,"4-it-s-abo","4-its-abo"),
           url = str_replace(url,"evil-director-s-cut","evil-directors-cut"),
           url = str_replace(url,"link-s-awakening","links-awakening"),
           url = str_replace(url,"links-awakening-dx","link-s-awakening-dx"),
           url = str_replace(url,"duelists-of-the-roses","duelists-of-the-roses-fa82a70b-8784-4e43-babc-9ba06b3ba75d"),
           url = str_replace(url,"--","-"),
           url = str_replace(url,"ing-harmony","ing-harmony--1"),
           url = str_replace(url,"fate-of-atlantis","fate-of-atlantis--1"),
           url = str_replace(url,"mike-tyson-s-punch-out","punch-out--2"),
           url = str_replace(url,"hd-15","hd-1-dot-5"),
           url = str_replace(url,"speedy-gonzales-in-los-gatos-banditos","speedy-gonzales-los-gatos-bandidos"),
           url = str_replace(url,"mega-man-zerozx-legacy-collection","mega-man-zero-slash-zx-legacy-collection"),
           url = str_replace(url,"dai-gyakuten-saiban-naruhodou-ryuunosuke-no-bouken","dai-gyakuten-saiban-naruhodou-ryuunosuke-no-bouken-1-and-2-best-price"),
           url = str_replace(url,"mario-s-time-machine","marios-time-machine"),
           url = str_replace(url,"sesame-street-elmo-s-number-journey","sesame-street-elmos-number-journey"),
           url = str_replace(url,"sesame-street-elmo-s-letter-adventure","sesame-street-elmos-letter-adventure--1"),
           url = str_replace(url,"25-remix","2-dot-5-remix"),
           url = str_replace(url,"sonic-adventure-dx-director-s-cut","sonic-adventure-dx-directors-cut"),
           url = str_replace(url,"shantae-and-the-pirate-s-curse","shantae-and-the-pirates-curse"),
           url = str_replace(url,"^(disney-s-)?goof-troop$","disneys-goof-troop"),
           url = str_replace(url,"marvel-s-spider-man","marvels-spider-man"),
           url = str_replace(url,"spyro-2-ripto-s-rage--reignited","spyro-2-riptos-rage-reignited"),
           url = str_replace(url,"clan-o-conall-and-the-crown-of-the-stag","clan-oconall-and-the-crown-of-the-stag"),
           url = str_replace(url,"ratchet-and-clank-up-your-arsenal","ratchet-clank-up-your-arsenal"),
           url = str_replace(url,"the-legend-of-zelda-ocarina-of-time--master-quest","the-legend-of-zelda-ocarina-of-time-master-quest"),
           url = str_replace(url,"mario-and-luigi","mario-luigi"),
           url = str_replace(url,"mario-luigi-superstar-saga-plus","mario-and-luigi-superstar-saga-plus"),
           url = str_replace(url,"mario-luigi-paper-jam","mario-and-luigi-paper-jam"),
           url = str_replace(url,"are-you-afraid-of-the-dark\\?-the-tale-of-orpheo-s-curse","are-you-afraid-of-the-dark-the-tale-of-orpheo-s-curse"),
           url = str_replace(url,"phoenix-wright-ace-attorney--dual-destinies","phoenix-wright-ace-attorney-dual-destinies"),
           url = str_replace(url,"kingdom-hearts-hd-28-final-chapter-prologue","kingdom-hearts-hd-2-dot-8-final-chapter-prologue"),
           url = str_replace(url,"phoenix-wright-ace-attorney--spirit-of-justice","phoenix-wright-ace-attorney-spirit-of-justice"),
           url = str_replace(url,"^[:graph:]{1}kami$","okami"),
           url = str_replace(url,"star-wars-jedi-knight--jedi-academy","star-wars-jedi-knight-jedi-academy"),
           url = str_replace(url,"man-and-bass","man-bass"),
           url = str_c("https://www.igdb.com/games/", url),
           game_id = sapply(url, search_game_id)) %>%
    dplyr::filter(game_id >= 0) %>%
    select(streamer, year, game_id, game, minutes, url)
  
  insert <- dbSendStatement(db, "INSERT INTO game_data (game_id, game, url) VALUES (?, ?, ?) ON CONFLICT DO NOTHING")
  dbBind(insert, list(as.vector(yearly_data$game_id), as.vector(yearly_data$game), as.vector(yearly_data$url)))
  
  insert <- dbSendStatement(db, "INSERT INTO yearly_playtime (streamer, year, game_id, minutes) VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING")
  dbBind(insert, list(as.vector(yearly_data$streamer), as.vector(yearly_data$year),
                      as.vector(yearly_data$game_id), as.vector(yearly_data$minutes)))
}




#Takes the name of a game as provided from the sullygnome csv, searches for it
#on igdb, and either returns the id number of the best match or -1 if no game
#match was found.
search_game_id <- function(name){
  tryCatch({POST(str_c(api_url, end_games),
                 add_headers(`Client-ID` = client_id,
                             Authorization = str_c("Bearer ", access_token)),
                 body=str_c('fields id, name; where url="', name, '";')) %>%
      use_series("content") %>%
      rawToChar() %>%
      fromJSON() %>%
      as_tibble() %>%
      magrittr::extract(1, 1) %>%
      unlist() %>%
      return()
  },
  #Some "games" on Twitch aren't actual games, such as "The Game Awards" or "Just Chatting"
  error = function(e) return(-1)) 
} 


#This can 10000% be apply family'd but I couldn't get it for some reason. No biggie.
for(i in 1:length(csvs)){
  construct_yearly_time(db, csvs[i])  
}

dbExecute(db, "DROP VIEW IF EXISTS cumulative_playtime;")
dbExecute(db, "CREATE VIEW IF NOT EXISTS cumulative_playtime AS
           SELECT streamer, game_id, SUM(minutes) AS minutes
           FROM yearly_playtime
           GROUP BY streamer, game_id")

################################################################################
#*
#*The below code chunk hard codes in some additional values for the coming year.
#*This is only being done in the interim until the construct_yearly_time function above is rewritten.
#*
################################################################################

past_results <- read_csv("momam7_base_data.csv")

games_list <- dbGetQuery(db, "SELECT DISTINCT game
           FROM game_data;")

to_add_names <- past_results$game[which(!past_results$game %in% unlist(games_list))]
to_add_urls <- c('https://www.igdb.com/games/super-mario-land-2-6-golden-coins',
                 'https://www.igdb.com/games/mega-man-3',
                 'https://www.igdb.com/games/mega-man-7',
                 'https://www.igdb.com/games/pokemon-card-gb2-great-rocket-dan-sanjou',
                 'https://www.igdb.com/games/pokemon-puzzle-league',
                 'https://www.igdb.com/games/wave-race-64',
                 'https://www.igdb.com/games/f-zero-x',
                 'https://www.igdb.com/games/contra-iii-the-alien-wars',
                 'https://www.igdb.com/games/the-simpsons-hit-run',
                 'https://www.igdb.com/games/kirby-and-the-forgotten-land',
                 'https://www.igdb.com/games/ninja-gaiden-ii-the-dark-sword-of-chaos',
                 'https://www.igdb.com/games/sonic-and-the-secret-rings',
                 'https://www.igdb.com/games/sonic-forces',
                 'https://www.igdb.com/games/monopoly-for-nintendo-switch',
                 'https://www.igdb.com/games/spongebob-squarepants-the-cosmic-shake',
                 'https://www.igdb.com/games/tony-hawks-pro-skater-3--1',
                 'https://www.igdb.com/games/bubsy-in-claws-encounters-of-the-furred-kind'
)
to_add_ids <- sapply(to_add_urls, search_game_id)

to_add_names <- 'Bubsy in Claws Encounters of the Furred Kind'
to_add_urls <- 'https://www.igdb.com/games/bubsy-in-claws-encounters-of-the-furred-kind'
to_add_ids <- sapply(to_add_urls, search_game_id)

to_add_names <- 'The Lord of the Rings: The Two Towers'
to_add_urls <- 'https://www.igdb.com/games/the-lord-of-the-rings-the-two-towers'
to_add_ids <- sapply(to_add_urls, search_game_id)

to_add_names <- 'Monopoly Plus'
to_add_urls <- 'https://www.igdb.com/games/monopoly-plus'
to_add_ids <- sapply(to_add_urls, search_game_id)


insert <- dbSendStatement(db, "INSERT INTO game_data (game_id, game, url) VALUES (?, ?, ?) ON CONFLICT DO NOTHING")
dbBind(insert, list(to_add_ids, to_add_names, to_add_urls))


################################################################################
#*
#* The following function will:
#* 1) Take every game pie and spike have ever streamed and construct a query that
#* will grab any potentially useful meta data from that query
#* 2) Send the query to the API
#* 3) Parse and normalize the result of the query
#* 4) Write the query to the MOMAM database.
#*
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

########################################################################################
#
# Below are helper functions used by the above function
#
########################################################################################

# Takes a list of game id numbers and returns a query that will search them.
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
}

games_run_query(db)

####################################################################################################
# I wish SQLite had 'CREATE OR REPLACE' statements.
dbExecute(db, "DROP VIEW IF EXISTS imputed_yearly_metadata;")
dbExecute(db, "DROP VIEW IF EXISTS yearly_metadata_playtime;")
dbExecute(db, "DROP VIEW IF EXISTS cumulative_metadata_playtime;")

# Some games don't have a release date or rating, so this view imputes one from the average.
dbExecute(db, "CREATE VIEW IF NOT EXISTS imputed_yearly_metadata AS
            SELECT id, name,
            ROUND(COALESCE(value, AVG(value) OVER(PARTITION BY name)), 0) value
            FROM games_metadata")


dbExecute(db, "CREATE VIEW IF NOT EXISTS yearly_metadata_playtime AS
           SELECT y.streamer, y.year, g.name, g.value, SUM(y.minutes) minutes
           FROM yearly_playtime y
           LEFT JOIN imputed_yearly_metadata g ON y.game_id = g.id
           GROUP BY y.streamer, y.year, g.name, g.value")


dbExecute(db, "CREATE VIEW IF NOT EXISTS cumulative_metadata_playtime AS
           SELECT streamer, name, value, SUM(minutes) minutes
           FROM yearly_metadata_playtime
           GROUP BY streamer, name, value;")




























