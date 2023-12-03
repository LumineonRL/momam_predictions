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
library(here)

connect_db <- function(folder, db_file) {
  return(dbConnect(SQLite(), dbname = str_c(folder, "/", db_file)))
}

load_all_csvs_from_folder <- function(folder) {
  str_c(folder, "/", list.files(folder)) %>%
    return()
}

# Pokemon games with multiple versions on Twitch are listed as one game, but
# the game database has them as individual entries.
# For example, on Twitch, "Pokemon Red/Blue" could refer to either "Pokemon Red"
# or "Pokemon Blue" on vgdb.
# This function always extracts the first game in this scenario.
pokemon_checker <- function(game) {
  if(str_detect(game, "Pokemon") & str_detect(game, "/")) {
    return(str_extract(game, "^[^/]+"))
  }
  else {
    return(game)
  }
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


#This is unfortunately EXTREMELY ugly since the way the URLs handle "'s" is
#inconsistent. It's pretty much a 50/50 whether "Mario's" will turn into
#"marios" or "mario-s" in the URL. &s and +s are also a nightmare.
#This could absolutely be cleaned up a bit but that's a non-trivial matter
#and a project for another day.
construct_yearly_time <- function(dbname, csv){
  message(paste("\nNow reading:", csv))
  
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
  dbClearResult(insert)
  
  insert <- dbSendStatement(db, "INSERT INTO yearly_playtime (streamer, year, game_id, minutes) VALUES (?, ?, ?, ?) ON CONFLICT DO NOTHING")
  dbBind(insert, list(as.vector(yearly_data$streamer), as.vector(yearly_data$year),
                      as.vector(yearly_data$game_id), as.vector(yearly_data$minutes)))
  dbClearResult(insert)
}
