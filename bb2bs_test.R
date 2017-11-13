suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(glue))
suppressMessages(library(stringr))
suppressMessages(library(nufflytics))

##Setup -----
league_name <- commandArgs(trailingOnly = T)[1]
testing <- length(commandArgs(trailingOnly = T)) > 1

api_key <- readRDS("data/api.key")

# Read in league parameters -----
params <- glue("data/{league_name}_parameters.csv") %>% 
  read_csv(col_types = "cccccciccllllll") %>% 
  as.list %>% 
  transpose(.names = .$ID)

# Find any new games for leagues ----- 

# Get most recent game, if uuid doesn't match with last recorded one, keep going back in league history until you find it then return all the new ones
get_new_games <- function(league_params, limit = 1, end = NA, cached_matches = list()) {
  
  new_games <- api_matches(
    key = api_key, 
    limit = limit,
    league = league_params$league, 
    competition = league_params$competition,
    platform = league_params$platform,
    end = end
  )
  
  if(!exists("matches", new_games)) return(NULL)
  
  matches <- c(cached_matches, new_games$matches)
  
  uuids <- matches %>% map(pluck, "uuid")
  
  if(league_params$last_game %in% uuids | is.na(league_params$last_game)) {
    new_idx <- match(league_params$last_game, uuids) - 1 # index of oldest unposted game
    
    #NA match means that league yet to be initialised, but we have found a match (otherwise would have returned NULL above)
    #So set index to 1 to return the only match we've found
    if(is.na(new_idx)) new_idx <- 1
    
    return(matches[0:new_idx])
  } else { # start requesting more games until you find the last one, 3 entries at a time (to reduce size of api requests)
    return(
      get_new_games(
        league_params, 
        #end = matches %>% map("started") %>% last() %>% lubridate::ymd_hms() %>% magrittr::subtract(lubridate::minutes(1)) %>% format(), # searches by start time for some reason
        #cached_matches = matches,
        limit = limit + 3
      )
    )
  }
  
}

new_games <- map(params, get_new_games)

# Extract full match data new games
get_league_matches <- function(league_new_games) {
  uuids_to_fetch <- map(league_new_games, pluck, "uuid")
  
  map(
    uuids_to_fetch,
    api_match,
    key = api_key
  )
}

new_match_data <- map(new_games, get_league_matches)

# Helper functions for formatting reports -----
# Add md markup to text
md <- function(text, markup) {glue("{markup}{text}{markup}")}

#Abbreviate team names
abbr <- function(name) {
  name %>%
    stringr::str_replace_all("\\[(.*)\\]","") %>% # strip out 'clan' tags
    stringr::str_replace_all("\\((.*)\\)", " ( \\1 )") %>% # Put spaces around brackets, so eg. USS Sulaco (REL Chapter) is abbreviated to US(RC)
    stringr::str_replace_all("([a-z_.-])([A-Z])", "\\1 \\2") %>%  # add a space before mid-word capitals and 'word separator' punctuation (_.-) followed by a capital
    stringr::str_replace_all("[&!,'\"*]",'') %>% # delete these characters
    abbreviate(1)
}

REBBL_races =  function(r) {switch(r,
                                   "Amazon" = "<:Zon:344918598286049281>",
                                   "Bretonnia" = "<:Bret:344918238976802826>",
                                   "Chaos" = "<:Chaos:344918252155305984>",
                                   "Chaos Dwarf" = "<:Chorf:344918276121427968>",
                                   "Dark Elf" = "<:Delf:344918286888337409>",
                                   "Dwarf" = "<:Dorf:344918297084559360>",
                                   "Elven Union" = "<:Pro:344918515817644033>",
                                   "Goblin" = "<:Gobbo:344918318685224975>",
                                   "Halfling" = "<:Fling:344918306236530698>",
                                   "High Elf" = "<:Helf:344918331930705921>",
                                   "Human" = "<:Human:344918344841035777>",
                                   "Khemri" = "<:Khemri:344918363438579714>",
                                   "Kislev Circus" = "<:Kislev:344918385542299648>",
                                   "Lizardmen" = "<:Lizard:344918404471455744>",
                                   "Necromantic" = "<:Necro:344918417712611328>",
                                   "Norse" = "<:Norse:344918434867314691>",
                                   "Nurgle" = "<:Nurgle:344918450977898501>",
                                   "Ogre" = "<:Ogre:344918473832660992>",
                                   "Orc" = "<:Orc:344918500583800845>",
                                   "Skaven" = "<:Rats:344918530531131403>",
                                   "Undead" = "<:Undead:344918543974006788>",
                                   "Underworld Denizens" = "<:UW:344918559417171970>",
                                   "Vampire" = "<:Vamp:344918571853414400>",
                                   "Wood Elf" = "<:Welf:344918583236755485>",
                                   r
)}

# Format individual fields of embed -----
format_stats <- function(match_data) {
  #team stats as the sum of player stats because surfs aren't recorded correctly in the cyanide summary stats
  team_stats <- match_data$match$teams %>% map("roster") %>% map(~map_df(., "stats") %>% summarise_all(sum)) 
  
  team_stats %<>% 
    bind_rows() %>% 
    select(
      TD = "inflictedtouchdowns",
      BLK = "inflictedtackles", 
      AVBr = "inflictedinjuries", 
      KO = "inflictedko",
      CAS = "inflictedcasualties", 
      KILL = "inflicteddead", 
      SURF = "inflictedpushouts", 
      INT = "inflictedinterceptions", 
      COMP = "inflictedpasses"
    ) %>% 
    t %>% 
    as.data.frame() %>% 
    mutate(stat = rownames(.)) %>% 
    select(stat, home=V1, away=V2) %>% 
    filter(c( TRUE, rowSums(.[-1,c("home","away")])>0 )) # Always keep TDs, then keep only if stats recorded
  
  #Format it into a table
  team_stats %>% 
    knitr::kable(
      col.names = c("", abbr(pluck(match_data, "teams", 1, "name")), paste0(abbr(pluck(match_data, "teams", 2, "name")), "  ")), 
      format = "pandoc", 
      align = "lrl"
    ) %>% 
    extract(-2) %>% # Remove knitr underlines
    collapse(sep = "\n") %>% #Make single string
    paste0("Python\n",.) %>% #Add code language marker to get some text colouring
    md("```") #Wrap in code block markup
}

format_injuries <- function(match_data) {
  player_info <- match_data$match$teams %>% map("roster") %>% set_names(match_data$teams %>% map_chr("name"))
  
  #If no new injuries or only BH, ignore. Otherwise list relevant characteristics
  parse_injuries <- function(player) {
    if(length(player$casualties_sustained_id) == 0 | all(player$casualties_sustained_id < 2)) return(NULL)
    
    player_data <- list(
      name = player$name,
      spp_old = player$xp,
      spp_gain = player$xp_gain,
      old_perms = player$casualties_state_id[-match(player$casualties_sustained_id,player$casualties_state_id)],
      new_injuries = player$casualties_sustained_id[player$casualties_sustained_id>1] 
    )
    
    #Deal with players with no skills
    if(length(player$skills) == 0)  {
      player_data$skills <- ""
    } else {
      player_data$skills = player$skills %>%
        stringr::str_replace_all(c("Increase" = "+", "Movement" = "MA", "Armour" = "AV", "Agility" = "AG", "Strength" = "ST")) %>% 
        stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    }
    
    # Fix up star players
    if (grepl("StarPlayer", player$type)) {
      player_data$type = "Star Player"
    } else {
      player_data$type = player$type %>% 
        stringr::str_replace("(.*)_(.*)", "\\2") %>% 
        stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    }
    
    player_data
  }
  
  #Filter out uninjured players and prepare the report text (removing teams that don't have any injuries)
  injury_data <- player_info %>% 
    modify_depth(2, parse_injuries) %>% 
    modify_depth(1,~.[!map_lgl(., is.null)]) %>% 
    modify_depth(1,~map(.,
                        glue_data,
                        '__{star_player_name(name)}__ *({type})*: {map_chr(new_injuries, id_to_casualty) %>% md("**") %>% collapse(", ")}
                        {collapse(c(skills, md(map_chr(old_perms, id_to_casualty), "*")), ", ")} ({spp_old+spp_gain} SPP)'
    )) %>%
    modify_depth(1, ~str_replace_all(.,c("\n,? *" ="\n", "Dead" = ":skull:", "\\(Star Player\\)" = ":star:"))) %>%
    modify_depth(1, ~collapse(., "\n\n")) %>%
    extract(map_lgl(., ~(length(.)>0)))
  
  #If injuries in game
  if(length(injury_data) > 0) {
    injury_data %>%
      imap(
        ~glue(
          "**{.y}**
          {.x}"
        )
      ) %>%
      collapse("\n\n")
  } else {NULL}
  
}

format_levels <- function(match_data) {
  player_info <- match_data$match$teams %>% map("roster") %>% set_names(match_data$teams %>% map_chr("name"))
  level_triggers <- c(6,16,31,51,76,176)
  
  did_level <- function(spp_before, spp_gain) {
    any(xor(spp_before >= level_triggers, spp_before+spp_gain >= level_triggers))
  }
  
  #If didn't level up this game, ignore. Otherwise list relevant characteristics
  parse_levels <- function(player) {
    if(!did_level(player$xp, player$xp_gain)) return(NULL)
    
    player_data <- list(
      name = player$name,
      spp_old = player$xp,
      spp_new = player$xp + player$xp_gain,
      perms = player$casualties_state_id[player$casualties_state_id>1] 
    )
    
    #Deal with players with no skills
    if(length(player$skills) == 0)  {
      player_data$skills <- ""
    } else {
      player_data$skills = player$skills %>%
        stringr::str_replace_all(c("Increase" = "+", "Movement" = "MA", "Armour" = "AV", "Agility" = "AG", "Strength" = "ST")) %>% 
        stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    }
    
    # Fix up star players
    if (grepl("StarPlayer", player$type)) {
      player_data$type = "Star Player"
    } else {
      player_data$type = player$type %>% 
        stringr::str_replace("(.*)_(.*)", "\\2") %>% 
        stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    }
    
    player_data
  }
  
  #Filter out unlevelled players and prepare the report text (removing teams that don't have any levels)
  level_data <- player_info %>% 
    modify_depth(2, parse_levels) %>% 
    modify_depth(1,~.[!map_lgl(., is.null)]) %>% 
    modify_depth(1,~map(.,
                        glue_data,
                        '__{name}__ *({type})*: **{spp_old} :arrow_right: {spp_new} SPP**
                        {collapse(c(skills, md(map_chr(perms,id_to_casualty), "*")), ", ")}'
    )) %>% 
    modify_depth(1, ~str_replace_all(.,c("\n,? *" ="\n", "\\(Star Player\\)" = ":star:"))) %>% 
    modify_depth(1, ~collapse(., "\n\n")) %>% 
    extract(map_lgl(., ~(length(.)>0)))
  
  # If injuries in game 
  if(length(level_data) > 0) {
    level_data %>% 
      imap(
        ~glue(
          "**{.y}**
          {.x}"
        )
      ) %>% 
      collapse("\n\n")
  } else {NULL}
  
}

calc_impact <- function(stats) {
  stats_to_collect <- c(
    "inflictedtackles" = "BLK", 
    "inflictedinjuries" = "AVBr", 
    "inflictedko" = "KO",
    "inflictedcasualties" = "CAS", 
    "inflicteddead" = "Kill", 
    "inflictedtouchdowns" = "TD",
    "inflictedpasses" = "Pass", 
    "inflictedmeterspassing" = "Pass_m",
    "inflictedcatches" = "Catch",
    "inflictedinterceptions" = "Int",
    "inflictedmetersrunning" = "Carry",
    "inflictedpushouts" = "Surf"
  )
  
  stats %<>% 
    extract(names(.) %in% names(stats_to_collect)) %>% 
    set_names(stats_to_collect[names(.)]) %>% 
    inset("FP",
          with(.,
               ceiling(BLK/5) + ceiling(AVBr/2) + KO + CAS + 2*Kill + 2*Surf + 3*TD + 2*Pass + ceiling(Pass_m/20)*(Pass>0) + 2*Catch + 5*Int + ceiling(Carry/50)
          )
    )
}

format_impact <- function(match_data, is_fantasy) {
  # Slightly different format to the other ones because have to keep team name as well 
  player_info <- match_data$match$teams %>% set_names(match_data$teams %>% map_chr("name"))
  
  parse_impact <- function(player, team_name) {
    player_data <- list(
      team = team_name,
      name = player$name,
      spp_gain = player$xp_gain,
      impact = calc_impact(player$stats)
    )
    
    # Fix up star players
    if (grepl("StarPlayer", player$type)) {
      player_data$type = "Star Player"
    } else {
      player_data$type = player$type %>% 
        stringr::str_replace("(.*)_(.*)", "\\2") %>% 
        stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2")
    }
    
    player_data
  }
  
  #Calc FP stats and order by FP, SPP gained, random. Then keep top 3
  impact_data <- player_info %>% 
    modify_depth(1, ~map2(.$roster, .$teamname, parse_impact)) %>%
    flatten %>%
    extract(
      order(
        map_dbl(., pluck, "impact", "FP"),
        map_dbl(., "spp_gain"),
        rnorm(length(.)),
        decreasing = TRUE
      )
    ) %>% 
    head(3)# %>% 
  # map(~inset(., "F_string", "")) # Empty string only populated later if using fantasy league
  
  #Add fantasy points explicitly if needed
  if(is_fantasy) {
    impact_data %<>%
      map(
        ~inset(.,
               "F_string",
               paste0("**",.$impact$FP ," FP **")
        )
      )
  } else {
    medals = c(":first_place:",":second_place:",":third_place:")
    impact_data %<>%
      imap(
        ~inset(.,
               "F_string",
               medals[.y]
        )
      )
  }
  
  impact_data %>%
    map(flatten) %>%
    map_chr(glue_data,
            '__{star_player_name(name)}__ *({abbr(team)} - {type})*: {F_string}\nBLK:{BLK}, AVBr:{AVBr}, KO:{KO}, CAS:{CAS}, Kill:{Kill}, Pass:{Pass} ({Pass_m}m), Catch:{Catch}, Int:{Int}, Surf:{Surf}, Carry:{Carry}m, TD:{TD}'
    ) %>% 
    str_replace_all(c(
      "(, )?(BLK|AVBr|KO|CAS|Kill|Catch|Int|Surf|Carry|TD):0m?" = "",
      "(, )?Pass:0 \\(.{1,}m\\)" = "",
      "\n, " = "\n",
      "- Star Player" = "- :star:"
    )) %>% 
    collapse("\n\n")
  
}

format_fields <- function(league_params, match_data) {
  fields <- list()
  if(league_params$stats) {
    fields %<>% append(list(list(
      name = "__**Game Stats**__", 
      value = format_stats(match_data),
      inline = T
    )))
  }
  
  if(league_params$injuries) {
    injuries <- format_injuries(match_data)
    
    if (!is.null(injuries)) {
      fields %<>% append(list(list(
        name = "__**Injury Report**__", 
        value = injuries,
        inline = T
      )))
    }
  }
  
  if(league_params$development) {
    level_ups <- format_levels(match_data)
    
    if (!is.null(level_ups)) {
      fields %<>% append(list(list(
        name = "__**Player Development**__", 
        value = level_ups,
        inline = T
      )))
    }
  }
  
  if(league_params$impact) {
    fields %<>% append(list(list(
      name = "__**Impact Players**__", 
      value = format_impact(match_data, league_params$fantasy),
      inline = T
    )))
  }
  
  fields
  
}

# Construct full embed with ladder position, etc. -----
competition_ladder <- function(league, competition, platform, this_uuid) {
  contests <- api_contests(key = api_key, league = league, competition = competition, status = "played", platform = platform)
  
  uuids <- contests$upcoming_matches %>% map_chr(pluck,"match_uuid")
  home <- contests$upcoming_matches %>% map(pluck, "opponents",1,"team") %>% transpose %>% as_data_frame() %>% mutate_all(simplify)
  away <- contests$upcoming_matches %>% map(pluck, "opponents",2,"team") %>% transpose %>% as_data_frame() %>% mutate_all(simplify)
  
  nufflytics:::interleave(bind_cols(home,away, uuid = uuids),bind_cols(away,home, uuid = uuids)) %>% 
    filter(uuid != this_uuid) %>% 
    mutate(Win = case_when(score>score1 ~ T, T~F), Tie = case_when(score == score1 ~ T, T~F), Loss = case_when(score1>score ~ T, T~F), TDdiff = score-score1) %>% 
    group_by(name) %>% 
    summarise_at(c("Win", "Tie","Loss", "TDdiff"), sum) %>% 
    mutate(Points = Win*3+Tie) %>% 
    arrange(desc(Points), desc(TDdiff)) %>% 
    mutate(Rank = suppressWarnings(ave(order(Points, TDdiff, decreasing = T), Points, TDdiff, FUN = min)))
}

format_title <- function(coaches) {
  glue("{coaches[[1]]$name} V {coaches[[2]]$name}")
}

format_description <- function(match_data, needs_ladder) {
  home_team <- match_data$match$teams[[1]]
  away_team <- match_data$match$teams[[2]]
  competition_standing <- ""
  
  if(needs_ladder) {
    
    placing <- function(place) {
      case_when(
        place == 1 ~ "1st",
        place == 2 ~ "2nd",
        place == 3 ~ "3rd",
        TRUE ~ paste0(place,"th")
      )
    }
    
    ladder <- competition_ladder(match_data$match$leaguename, match_data$match$competitionname, match_data$match$platform, match_data$uuid)
    
    home_ranking <- filter(ladder, name == home_team$teamname)
    away_ranking <- filter(ladder, name == away_team$teamname)
    
    if(nrow(home_ranking) == 0) home_ranking <- data_frame(name = home_team$teamname, Win = 0, Tie = 0, Loss = 0, Rank = 0)
    if(nrow(away_ranking) == 0) away_ranking <- data_frame(name = away_team$teamname, Win = 0, Tie = 0, Loss = 0, Rank = 0)
    
    competition_standing = glue("\n\n{home_ranking$Win}-{home_ranking$Tie}-{home_ranking$Loss}{ifelse(home_ranking$Rank>0,str_c(' ', placing(home_ranking$Rank)),'')} V {ifelse(away_ranking$Rank > 0, str_c(placing(away_ranking$Rank),' '),'')}{away_ranking$Win}-{away_ranking$Tie}-{away_ranking$Loss}\n")
  }
  
  if (home_team$score > away_team$score) {home_team$teamname %<>%  md("**")}
  if (away_team$score > home_team$score) {away_team$teamname %<>%  md("**")}
  
  glue(
    "{home_team$teamname} V {away_team$teamname}
TV {home_team$value} {id_to_race(home_team$idraces)}{ifelse(league_name == 'REBBL',str_c(' ',REBBL_races(id_to_race(home_team$idraces))),'')} V {ifelse(league_name == 'REBBL',str_c(REBBL_races(id_to_race(away_team$idraces)),' '), '')}{id_to_race(away_team$idraces)} {away_team$value} TV {competition_standing}
{md(match_data$match$competitionname,'*')}"
  )
}

format_embed <- function(league_params, match_data) {
  list(
    list(
      title = format_title(match_data$coaches),
      description = format_description(match_data, league_params$ladder),
      url = glue("http://www.mordrek.com/goblinSpy/web/game.html?mid={match_data$uuid}"),
      color = league_params$colour,
      fields = format_fields(league_params, match_data)
    )
  )
}


# Post matches to discord
post_match <- function(league_params, match_data) {
  
  #nbsupporters = 0 means admin decided game, mvps = 0|2 means conceded game
  if(pluck(match_data, "match", "teams", 1, "nbsupporters") == 0 | pluck(match_data, "match", "teams", 1, "mvp") != 1) return(NULL)
  
  response <- httr::POST(
    url = league_params$webhook,
    body = list(
      username = league_params$username,
      avatar_url = league_params$avatar,
      embeds = format_embed(league_params, match_data)
    ),
    encode = "json"
  )
  
  if (response$status_code == 429) { #rate limited
    wait_time <- httr::content(response)$retry_after
    print(glue("Rate limited, pausing for {wait_time} seconds."))
    Sys.sleep(wait_time)
  }
  
  if (response$status_code == 204) { #all good, just wait a sec in case there's another one
    Sys.sleep(1)
  }
  
  response
}

post_matches <- function(league_params, matches) {
  map(matches, ~post_match(league_params, .))
}


responses <- map2(params, new_match_data, post_matches)

#Complete, so update with new game uuids (if a more recent game is found)
if(!testing){
  last_uuid <- new_games %>% map(pluck,1,"uuid") %>% map_chr(fill_nulls)
  has_new_match <- map_lgl(last_uuid, Negate(is.na))
  
  params %>% 
    transpose %>% 
    as_data_frame() %>% 
    mutate_all(as.character) %>% 
    mutate(last_uuid, has_new_match, last_game = ifelse(has_new_match, last_uuid, last_game)) %>% 
    select(-last_uuid, -has_new_match) %>% 
    write_csv(glue("data/{league_name}_parameters.csv"))
}