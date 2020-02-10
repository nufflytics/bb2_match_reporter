source("report.R")
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(glue))
suppressMessages(library(stringr))
suppressMessages(library(nufflytics))

api_key <- readRDS("data/api.key")

# This will post all with rebbl.net team links
league_key = "1siRNzFH3hawaQn4P4c3ukSj23NDwM4hF_hDNZadYOL4"


#* @get /report/<uuid>
function(uuid, req, res) {
  
  # Check redirect data
  clan_hooks <- readRDS("data/clan_hooks.rds")
  
  race_hooks <- readRDS("data/race_hooks.rds")
  
  # Get match data
  match_data <- api_match(api_key, match_id = uuid)
  
  # Work out where to post
  all_params <- read_tsv("settings/league_params.tsv") %>% 
    mutate(last_game = str_remove(last_game, "#")) %>% 
    as.list %>% 
    transpose(.names = .$ID) %>% 
    keep(!is.na(names(.))) %>% #remove empty rows
    map(~modify_at(.,"colour", ~(.x %>% str_replace("#","") %>% as.hexmode() %>% as.integer())))
  
  league_params <- all_params %>% 
    # Right league
    keep(~str_detect(glue::glue("{.$league}[,^]"), match_data$match$leaguename)) %>% 
    # If competition specified, right competition
    keep(~ifelse(
      is.na(.$competition), 
      T, 
      str_detect(glue::glue("{.$competition}[,^]"), match_data$match$competitionname)
      ))
  
  if(length(league_params) == 0) {
    #res$status_code <- 404
    res$body <- glue::glue("Error: Can't find league posting parameters for match id {uuid}")
    
    return(res)
  }
  
  if(length(league_params) > 1) {
    #res$status_code <- 404
    res$body <- glue::glue("Error: Multiple league posting parameters found for match id {uuid}")
    
    return(res)
  }
  
  # Know we only haave one league posting parameters now
  league_params <- league_params[[1]]
  
  #Post it
  
  posted <- post_match(league_params, match_data, check_clans = T, check_race = T)
  
  res
}
