suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(glue))
suppressMessages(library(stringr))
suppressMessages(library(nufflytics))
suppressMessages(library(googlesheets))

##Setup -----
league_key <- commandArgs(trailingOnly = T)[1]
testing <- length(commandArgs(trailingOnly = T)) > 1

#Check if already running
if(length(list.files(path = "data/lock/", pattern = glue("{league_key}.lock"))) > 0) {
  stop(lubridate::now(), " Already running")
} else {
  write_file("",glue("data/lock/{league_key}.lock"))
}

test_type <- ""
if(testing) test_type = commandArgs(trailingOnly = T)[2]

api_key <- readRDS("data/api.key")

clan_hooks <- readRDS("data/clan_hooks.rds")

race_hooks <- readRDS("data/race_hooks.rds")

source("report.R")

gs_auth(token = "data/token.rds")
params_sheet <-gs_key(league_key)

# Read in league parameters -----
read_params <- function(gsheet) {
  gsheet %>% 
    gs_read(ws = "Settings", col_types = "ccccccccllllllc", trim_ws = F) %>% 
    mutate(last_game = str_remove(last_game, "#")) %>% 
    as.list %>% 
    transpose(.names = .$ID) %>% 
    keep(!is.na(names(.))) %>% #remove empty rows
    map(~modify_at(.,"colour", ~(.x %>% str_replace("#","") %>% as.hexmode() %>% as.integer()))) # convert hexcodes into integer colours
}

params <- read_params(params_sheet)

# Find any new games for leagues ----- 

# get_last_match <- function(league_params) {
#   league_ret <- api_league(api_key, league = league_params$league, platform = league_params$platform)
#   
#   if( "date_last_match" %in% names(league_ret$league)) {
#     return(league_ret$league$date_last_match)
#   } else {
#     return(FALSE)
#   }
# }
# 
#last_matches_date <- map_chr(params, get_last_match)

new_games <- map(params, get_new_games)

new_match_data <- map(new_games, get_league_matches)

if(test_type != "update") { # don't post if all you want to do is update to the latest match
  responses <- map2(params, new_match_data, post_matches)
}

#Complete, so update with new game uuids (if a more recent game is found)----
if(!testing | test_type == "update"){
  
  newest_game <- function(match_list) {
    if(is_empty(match_list)) return(NA)
    match_list %>% map_chr("uuid") %>% .[[1]]
  }
  
  last_uuid <- new_games %>% map_chr(newest_game) 
  has_new_match <- map_lgl(last_uuid, Negate(is.na))
  
  #refetch params from sheet in case they have been edited
  params <- read_params(params_sheet)
  
  #update with latest game id and rerwite to google sheet
  params %>% 
    transpose %>% 
    as_data_frame() %>% 
    mutate_all(as.character) %>% 
    mutate(
      last_uuid, 
      has_new_match, 
      last_game = ifelse(has_new_match, last_uuid, last_game) %>% str_c("#",.), 
      colour = colour %>% as.integer() %>% as.hexmode() %>% format(width=6) %>% str_c("#",.)
    ) %>% 
    select(-last_uuid, -has_new_match) %>% 
    gs_edit_cells(params_sheet, ws = "Settings", input=.)
}

system2("rm", glue("data/lock/{league_key}.lock"))
