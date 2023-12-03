
lookup_id_names <- function(url=api_url,endpoint,info=game_info,clientid=client_id,accesstoken=access_token,column,database,table){
  
  #ids of the categories that will be looked up
  ids <- info %>%
    pluck(column) %>%
    unlist() %>%
    unique() %>%
    sort()
  
  #Each API query is capped at 500 results, so the queries need to be broken up into
  #batches of 500 in the event that more results need to be searched.
  for(i in 1:ceiling(length(ids)/500)){
    start <- ((i-1)*500+1)
    end <- i*500
    
    query <- ids[start:end] %>%
      unique() %>% #Multiple NA values appear when end is not a multiple of 500. This reduces that to 1.
      as_tibble() %>%
      as.character() %>%
      str_remove("c") %>% #String is currently of the form c(A, \nB, ..., NA)
      str_remove_all("\\n") %>%
      str_remove(", NA") %>%
      str_c('fields id,slug; where id = ',.,'; limit 500;')
    
    tryCatch({
      df <- POST(str_c(url,endpoint),
                 add_headers(`Client-ID`=clientid,
                             Authorization=str_c("Bearer ",accesstoken)),
                 body=query) %>%
        use_series("content") %>%
        rawToChar() %>%
        fromJSON() %>%
        as_tibble() %>%
        arrange(id)
    },error=function(e){
      message("Query returned no results.")
      stop()
    })
    
    #Only append for values 500+
    if(i > 1){
      dbWriteTable(database,table,df,append=T)
    }
    else{
      dbWriteTable(database,table,df,overwrite=T)
    }
  }
}

lookup_id_names(endpoint=end_engines,column="game_engines",database=db,table="games_engines")
lookup_id_names(endpoint=end_keywords,column = "keywords",database = db,table="games_keywords")
lookup_id_names(endpoint=end_themes,column = "themes",database = db,table="games_themes")
lookup_id_names(endpoint=end_franchises,column = "franchises",database = db,table="games_franchises")
lookup_id_names(endpoint=end_perspectives,column = "player_perspectives",database = db,table="games_perspectives")
lookup_id_names(endpoint=end_genres,column = "genres",database = db,table="games_genres")
lookup_id_names(endpoint=end_platforms,column = "platforms",database = db,table="games_platforms")
lookup_id_names(endpoint=end_age,column = "age_ratings",database = db,table="games_ratings")

#Company is done separately from the automated function since it's a mutli-step
#process. We take the involved companies, filter down to the developers, and then
#execute another search to retrieve the relevant company names.
involved_companies <- game_info$involved_companies %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  as_tibble() %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,company,developer; where id = ',.,' & developer = true; limit 500;') %>%
  POST(str_c(api_url,end_company),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body=.) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()

company_names <- involved_companies %>%
  pluck("company") %>%
  unlist() %>%
  sort() %>%
  as_tibble() %>%
  as.character() %>%
  str_remove("c") %>%
  str_c('fields id,slug; where id = ',.,'; limit 500;') %>%
  POST(str_c(api_url,"/companies"),
       add_headers(`Client-ID`=client_id,
                   Authorization=str_c("Bearer ",access_token)),
       body=.) %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()


dbWriteTable(db,"games_involved_companies",involved_companies,overwrite=T)
dbWriteTable(db,"games_companies",company_names,overwrite=T)

#And finally, age ratings. It's actually faster to just hard code these in using
#the igdb API docs https://api-docs.igdb.com/#age-rating
ratings <- tibble(id=c(1,2,3,4,5,6,7,8,9,10,11,12),
                  slug=c("three","seven","twelve","sixten","eighteen",
                         "rp","ec","e","e10","t","m","ao"))

dbWriteTable(db,"games_ratings",ratings,overwrite=T)

##############################################################################

#Helper function that will be used when converting categorical columns to
#dummy variables.
#This is used since some columns are actually a list of multiple categorical
#values. Traditional dummy variable encoding function, unfortunately, expect
#each column to contain a single category and nothing else.
mSplit <- function(vec) {
  if (!is.character(vec))
    vec <- as.character(vec)
  L <- strsplit(vec, "-")
  ids <- unlist(lapply(seq_along(L), function(i) rep(i, length(L[[i]])) ))
  U <- sort(unique(unlist(L)))
  M <- matrix(0, nrow = length(vec), 
              ncol = length(U), 
              dimnames = list(NULL, U))
  M[cbind(ids, match(unlist(L), U))] <- 1L
  M
}

#Takes in a data frame with an id column and a column with a list of ids.
#column_prefix will be used for the new column names.
#table_with_info is the db table that contains id and slug info for every
#id in the column of interest.
categorical_to_dummy <- function(df,id_column,data_column,column_prefix,db,table_to_write,table_with_info){
  table <- dbGetQuery(db,str_c("SELECT * FROM ",table_with_info)) %>%
    as_tibble() %>%
    mutate(id = as.character(id))
  
  id_string <- table$id
  slug_string <- table$slug
  
  dummy_df <- df %>%
    mutate(new_column = as.character(eval(as.name(paste(data_column)))),
           new_column = case_when(new_column=="NULL"~"",
                                  T~new_column)) %>%  
    select(new_column) %>%
    unlist() %>%
    stri_replace_all_regex("\\b" %s+% id_string %s+% "\\b",slug_string,vectorize_all=F) %>%
    str_replace_all("-","_") %>%
    str_remove_all("(c\\(|\\))") %>%
    str_replace_all(":",", ") %>%
    str_replace_all(", ","-") %>%
    as.factor() %>%
    mSplit() %>%
    as_tibble() %>%
    setnames(.,names(.),paste0(column_prefix,names(.)))
  
  df_to_write <- df %>%
    pluck(id_column) %>%
    cbind(dummy_df) %>%
    setnames(.,names(.)[1],"id") #First column would be named "." otherwise.
  
  #The max number of columns for a sqlite table is (by default) 999.
  #This stores the tables in batches if there are more than 999 categories.
  if(ncol(df_to_write)>999){
    for(i in 1:ceiling(ncol(df_to_write)/999)){
      to_append <- str_c("_",as.character(i))
      
      #The first column will be the id column
      if(i == 1){
        start <- 2
        end <- 999
      }
      #The final subset may have less than 999 columns.
      else if(i == ceiling(ncol(df_to_write)/999)){
        message("Attempting to write a table with more than 999 columns. The values will be split across multiple tables.")
        start <- start + 998
        end <- ncol(df_to_write)
      }
      else{
        start <- start + 998
        end <- end + 998
      }
      
      #Always keep id column
      df_subset <- df_to_write[,c(1,start:end)]
      
      dbWriteTable(db,str_c(table_to_write,to_append),df_subset,overwrite=T)
      
    }
  }
  else{
    dbWriteTable(db,table_to_write,df_to_write,overwrite=T)  
  }
  
}

#Could mapply this, but I think this actually ends up being easier to read.
categorical_to_dummy(game_info,"id","game_engines","engine_",db,"data_engines","games_engines")
categorical_to_dummy(game_info,"id","genres","genre_",db,"data_genres","games_genres")
categorical_to_dummy(game_info,"id","keywords","keyword_",db,"data_keywords","games_keywords")
categorical_to_dummy(game_info,"id","platforms","platform_",db,"data_platforms","games_platforms")
categorical_to_dummy(game_info,"id","player_perspectives","perspective_",db,"data_perspectives","games_perspectives")
categorical_to_dummy(game_info,"id","themes","theme_",db,"data_themes","games_themes")
categorical_to_dummy(game_info,"id","age_ratings","rating_",db,"data_ratings","games_ratings")
categorical_to_dummy(game_info,"id","franchises","franchise_",db,"data_franchises","games_franchises")

#Companies once again require special treatment.
company_table <- dbGetQuery(db,"SELECT i.id, c.slug
           FROM games_involved_companies as i
           LEFT JOIN games_companies as c
           ON i.company = c.id") %>%
  as_tibble()

id_string <- company_table$id
slug_string <- company_table$slug

#Involved companies includes not just developers, but publishers and distributors as well.
#However, the developer has a much larger impact on who will win a particular game
#than the publisher does, meaning any publishers can be ignored.
#The normal stringi_replace_all function won't successfully replace all company ids
#Because some of those ids are for publishing companies.
game_info %>%
  mutate(new_column = as.character(involved_companies),
         new_column = case_when(new_column=="NULL"~"",
                                T~new_column)) %>%  
  select(new_column) %>%
  unlist() %>%
  stri_replace_all_regex("\\b" %s+% id_string %s+% "\\b",slug_string,vectorize_all=F) %>%
  str_replace_all("-","_") %>% #We're going to be removing all -'s later.
  str_remove_all("(c\\(|\\))") %>%
  str_replace_all(":",", ") %>%
  str_replace_all(", ","-") %>%
  str_remove_all("[0-9]{2,}") %>% #Remove ids for companies that are not developers 
  str_replace_all("-{2,}","-") %>% #Clean up the debris left behind by removing the publisher ids.
  str_remove_all("^-|-$") %>% #Remove -s at the beginning and end of string.
  as.factor() %>%
  mSplit() %>%
  as_tibble() %>%
  setnames(.,names(.),paste0("company_",names(.))) %>%
  cbind(game_info$id,.) %>%
  #This *could* be done in the previous setnames call, but it'd be ugly.
  setnames(.,names(.)[1],"id") %>% 
  dbWriteTable(db,"data_companies",.,overwrite=T)


#Determines how much time (in minutes) each streamer played a certain piece of metadata
#(genre, console, etc) for a give year. For example, calculate how much time
#pie spent playing adventure games in 2015.
get_annual_playtime <- function(database=db,db_table_year,data_table,columns){
  dbGetQuery(database,str_c("SELECT p.game,p.minutes, d.*
               FROM ",db_table_year," AS p
               LEFT JOIN ",data_table," AS d ON p.game_id = d.id")) %>%
    mutate_at(vars(starts_with(columns)),funs(.*minutes)) %>%
    summarise_at(vars(starts_with(columns)),sum) %>%
    dbWriteTable(db,str_c(db_table_year,str_extract(data_table,"_[:alnum:]+")),.,overwrite=T) 
}

#Faster and neater to mapply this than to write out the get_annual_playtime function 72 times.
mapply(get_annual_playtime,
       db_table_year=c(rep("pie2015",6),rep("pie2016",6),rep("pie2017",6),
                       rep("pie2018",6),rep("pie2019",6),rep("pie2020",6),
                       rep("spike2015",6),rep("spike2016",6),rep("spike2017",6),
                       rep("spike2018",6),rep("spike2019",6),rep("spike2020",6)),
       data_table=rep(c("data_themes","data_franchises","data_companies",
                        "data_genres","data_perspectives","data_platforms"),12),
       columns=rep(c("theme","franchise","compan","genre","perspective","platform"),12))



#Uses the yearly playtime to calculate cumulative playtime.
get_cumulative_playtime <- function(database=db,streamer,columns,table_ends_with){
  dbGetQuery(database,str_c("SELECT * FROM ",streamer,"2015_",table_ends_with," UNION
               SELECT * FROM ",streamer,"2016_",table_ends_with," UNION
               SELECT * FROM ",streamer,"2017_",table_ends_with," UNION
               SELECT * FROM ",streamer,"2018_",table_ends_with," UNION
               SELECT * FROM ",streamer,"2019_",table_ends_with," UNION
               SELECT * FROM ",streamer,"2020_",table_ends_with,"")) %>%
    tibble(year=c(2015,2016,2017,2018,2019,2020),.) %>%
    mutate_at(vars(starts_with(columns)), cumsum) %>%
    dbWriteTable(database,str_c(streamer,"_cumulative_",table_ends_with),.,overwrite=T)
}


mapply(get_cumulative_playtime,
       streamer=c(rep("pie",6),rep("spike",6)),
       columns = rep(c("theme","franchise","compan","genre","perspective","platform"),2),
       table_ends_with=rep(c("themes","franchises","companies","genres","perspectives","platforms"),2))

dbDisconnect(db)



franchises <- dbGetQuery(db,"SELECT n.name, f.*
           FROM games_name AS n
           LEFT JOIN data_franchises AS f ON n.id = f.id") %>%
  as_tibble()

contain


names(franchises)[68]
franchises[,c(1,2,68)] %>%
  filter(str_detect(name,"Zero")) %>%
  View()



dbGetQuery(db,"SELECT n.name, f.*
           FROM games_name AS n
           LEFT JOIN data_franchises AS f ON n.id = f.id") %>%
  mutate_at(vars(contains("banjo")),
            funs(. = case_when(str_detect(name,"Banjo")~1,
                               T~0
            ))) %>%
  View()


POST(str_c(api_url,end_games),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body='fields *; where id = 1028;') %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble() %>%
  View()


POST(str_c(api_url,end_franchises),
     add_headers(`Client-ID`=client_id,
                 Authorization=str_c("Bearer ",access_token)),
     body='fields id; where slug = "sonic-the-hedgehog";') %>%
  use_series("content") %>%
  rawToChar() %>%
  fromJSON() %>%
  as_tibble()
