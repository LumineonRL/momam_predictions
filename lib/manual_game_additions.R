manual_db_additions <- function(db) {
  past_results <- read_csv(str_c(here("data/momam7"), "/momam7_base_data.csv"))
  
  games_list <- dbGetQuery(db, "SELECT DISTINCT game
           FROM game_data;")
  
  # Looks for games not in the game list of past and upcoming games that are not
  # already in the data base.
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
  
  # Some games have names that differ from the csvs and their corresponding URLs and
  # need special treatment. This game, for example, is typically simply referred to as "Bubsy"
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
  dbClearResult(insert)
}
