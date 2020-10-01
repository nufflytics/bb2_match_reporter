rebbl_emotify <- function(s) {
  s %>% stringr::str_replace_all(c(
    "\n\n+"="\n\n", 
    ":[D|d]ead:"="<:Dead:311936561069555712>", 
    ":AtkDown:"="<:AtkDown:311936485098258442>",
    ":Ogre:" = "<:Ogre:344918473832660992>",
    ":Blitz:" = "<:Blitz:311936522121117706>",
    ":Injury:" = "<:Injury:311936638626299904>",
    ":DefDown:" = "<:DefDown:311936579478355978>",
    "\\(Star Player\\)" = ":star:",
    "\\[col.*?\\]" = ""
  ))
}

uuid_to_id <- function(uuid) {
  if(is.na(uuid)) return(0)
  uuid %>% str_sub(3) %>% as.hexmode() %>% as.integer()
}


id_to_uuid <- function(id, platform) {
  pcode <- switch(platform,
                  "pc" = "10",
                  "ps4" = "11",
                  "xb1" = "12"
  )
  
  paste0(pcode, id %>% as.hexmode() %>% format(width = 8))
}

BAD_WORDS <- c(
  "CUNT",
  "FUCK"
)

#Get most recent game, if uuid doesn't match with last recorded one, keep going back in league history until you find it then return all the new ones
#This should be more efficient, fix it if Cyanide decide to change the way the /matches api works
get_new_games <- function(league_params, limit = 50) {
  new_games <- api_matches(
    key = api_key,
    limit = limit,
    league = league_params$league,
    competition = league_params$competition,
    platform = league_params$platform
  )
  
  if(is_logical(new_games)) return(NULL) #api returns false if no competitions started in the league
  if(!exists("matches", new_games)) return(NULL) #if no games played, no $matches in the response
  
  match_table <- data_frame(
    id = map_int(new_games$matches,"id"), 
    end_time = map_chr(new_games$matches, "finished") %>% lubridate::ymd_hms(), 
    data = new_games$matches
  ) %>% 
    arrange(desc(end_time))
  last_seen_id <- uuid_to_id(league_params$last_game)
  
  #Check if we have older games than previously seen - ie. if we have gone back far enough to know we have found 'all'(?) new matches (or if this is the first time)
  #Or if all we want is to update to the latest game without posting anything
  #Or if we have hit the 50 game search limit and just want to process the games found that far back
  if(any(match_table$id <= last_seen_id) | is.na(league_params$last_game) | test_type == "update" | limit > 50) {
    unposted_matches <- filter(match_table, id > last_seen_id)
    
    return(unposted_matches$data)
  } else { # start requesting more games until you find the last one, 20 entries at a time (to reduce size of api requests)
    
    return(
      get_new_games(
        league_params,
        #end = matches %>% map("started") %>% last() %>% lubridate::ymd_hms() %>% magrittr::subtract(lubridate::minutes(1)) %>% format(), # searches by start time for some reason
        #cached_matches = matches,
        limit = limit + 20
      )
    )
  }
}

# Extract full match data new games
get_league_matches <- function(league_new_games) {
  uuids_to_fetch <- map(league_new_games, pluck, "uuid")
  
  map(
    uuids_to_fetch,
    api_match,
    key = api_key
  )
}

# Helper functions for formatting reports -----
# Add md markup to text
md <- function(text, markup) {glue("{markup}{text}{markup}")}

#Abbreviate team names
abbr <- function(name) {
  abb <- name %>%
    stringr::str_replace("\\[(.*?)\\]","") %>% # strip out 'clan' tags
    stringr::str_replace_all("\\((.*?)\\)", " ( \\1 )") %>% # Put spaces around brackets, so eg. USS Sulaco (REL Chapter) is abbreviated to US(RC)
    stringr::str_replace_all("([a-z_.-])([A-Z])", "\\1 \\2") %>%  # add a space before mid-word capitals and 'word separator' punctuation (_.-) followed by a capital
    stringr::str_replace_all("[\\[\\]&!,'\"*:]",'') %>% # delete these characters
    abbreviate(1)
  
  #sigh
  if (any(stringr::str_detect(abb, BAD_WORDS))) {
    return("o_O")
  }
  else {
    return(abb)
  }
}

REBBL_races =  function(r, is_clan) {
  if (!is_clan) {
    switch(r,
           "Amazon" = "<:Zon:344918598286049281>",
           "Brettonian" = "<:Bret:344918238976802826>",
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
           ""
    )
  } else {
    switch(r,
           "Amazon" = "<:Zon:620145744485154817>",
           "Brettonian" = "<:Bret:620145741821640705>",
           "Chaos" = "<:Chaos:620145741742080001>",
           "Chaos Dwarf" = "<:Chorf:620145743792963584>",
           "Dark Elf" = "<:Delf:620145743381921802>",
           "Dwarf" = "<:Dorf:620145743746826260>",
           "Elven Union" = "<:Pro:620145743298035712>",
           "Goblin" = "<:Gobbo:620145743696494632>",
           "Halfling" = "<:Fling:620145743545499659>",
           "High Elf" = "<:Helf:620145743482585088>",
           "Human" = "<:Human:620145743629385738>",
           "Khemri" = "<:Khemri:620145743356755983>",
           "Kislev Circus" = "<:Kislev:620145743315075073>",
           "Lizardmen" = "<:Lizard:620145743839232011>",
           "Necromantic" = "<:Necro:620145743159885830>",
           "Norse" = "<:Norse:620145745244454912>",
           "Nurgle" = "<:Nurgle:620145743751151626>",
           "Ogre" = "<:Ogre:620145743700819978>",
           "Orc" = "<:Orc:620145743780380692>",
           "Skaven" = "<:Rats:620145744313188354>",
           "Undead" = "<:Undead:620145745353506816>",
           "Underworld Denizens" = "<:UW:620145743759671306>",
           "Vampire" = "<:Vamp:620145744682156032>",
           "Wood Elf" = "<:Welf:620145743616802816>",
           ""
    )
  }
}

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
      col.names = c("", abbr(pluck(match_data, "match", "teams", 1, "teamname")), paste0(abbr(pluck(match_data, "match", "teams", 2, "teamname")), "  ")), 
      format = "pandoc", 
      align = "lrl"
    ) %>% 
    extract(-2) %>% # Remove knitr underlines
    glue::glue_collapse(sep = "\n") %>% #Make single string
    paste0("Python\n",.) %>% #Add code language marker to get some text colouring
    md("```") #Wrap in code block markup
}

format_injuries <- function(match_data) {
  player_info <- match_data$match$teams %>% map("roster") %>% set_names(match_data$match$teams %>% map_chr("teamname"))
  
  #If no new injuries or only BH, ignore. Otherwise list relevant characteristics
  parse_injuries <- function(player) {
    if(length(player$casualties_sustained_id) == 0 | all(player$casualties_sustained_id < 2)) return(NULL)
    
    player_data <- list(
      name = player$name,
      spp_old = player$xp,
      spp_gain = player$xp_gain,
      old_perms = player$casualties_state_id[-match(player$casualties_sustained_id,player$casualties_state_id, nomatch = 0)],
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
    map(~map(.,parse_injuries)) %>% 
    map(~.[!map_lgl(., is.null)]) %>% 
    map(~map(.,
             glue_data,
             '__{star_player_name(name)}__ *({type})*: {map_chr(new_injuries, id_to_casualty) %>% md("**") %>% glue::glue_collapse(", ")}
                        {glue::glue_collapse(c(skills, md(map_chr(old_perms, id_to_casualty), "*")), ", ")} ({spp_old+spp_gain} SPP)'
    )) %>%
    map(~str_replace_all(.,
                         c("\n,? *" ="\n", 
                           "\\(Star Player\\)" = ":star:")
    )) %>%
    map(~glue::glue_collapse(., "\n\n")) %>%
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
      glue::glue_collapse("\n\n")
  } else {NULL}
  
}

format_levels <- function(match_data) {
  player_info <- match_data$match$teams %>% map("roster") %>% set_names(match_data$match$teams %>% map_chr("teamname"))
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
    map(~map(., parse_levels)) %>% 
    map(~.[!map_lgl(., is.null)]) %>% 
    map(~map(.,
             glue_data,
             '__{star_player_name(name)}__ *({type})*: **{spp_old} :arrow_right: {spp_new} SPP**
                        {glue::glue_collapse(c(skills, md(map_chr(perms,id_to_casualty), "*")), ", ")}'
    )) %>% 
    map(~str_replace_all(.,c("\n,? *" ="\n", "\\(Star Player\\)" = ":star:"))) %>% 
    map(~glue::glue_collapse(., "\n\n")) %>% 
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
      glue::glue_collapse("\n\n")
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
  player_info <- match_data$match$teams %>% set_names(match_data$match$teams %>% map_chr("teamname"))
  
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
    map(~map2(.$roster, .$teamname, parse_impact)) %>%
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
    glue::glue_collapse("\n\n")
  
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
        value = injuries %>% rebbl_emotify(),
        inline = T
      )))
    }
  }
  
  if(league_params$development) {
    level_ups <- format_levels(match_data)  
    
    if (!is.null(level_ups)) {
      fields %<>% append(list(list(
        name = "__**Player Development**__", 
        value = level_ups %>% rebbl_emotify(),
        inline = T
      )))
    }
  }
  
  if(league_params$impact) {
    fields %<>% append(list(list(
      name = "__**Impact Players**__", 
      value = format_impact(match_data, league_params$fantasy) %>% rebbl_emotify(),
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

format_teamname <- function(team, match_data) {
  t <- NULL
  
  if (league_key == "1siRNzFH3hawaQn4P4c3ukSj23NDwM4hF_hDNZadYOL4") {
    t <- glue::glue("[{team$teamname}](http://rebbl.net/rebbl/team/{team$idteamlisting})")
  } else {
    t <- team$teamname
  }
  
  t %>% rebbl_emotify()
}

format_division <- function(match_data) {
  if (league_key == "1siRNzFH3hawaQn4P4c3ukSj23NDwM4hF_hDNZadYOL4") {
    glue::glue("[{md(match_data$match$competitionname,'*')}](http://rebbl.net/rebbl/{URLencode(match_data$match$leaguename %>% str_remove('REBBL - '))}#{URLencode(match_data$match$competitionname)})")
  } else {
    md(match_data$match$competitionname,'*')
  }
}

format_description <- function(match_data, needs_ladder, redirected = F) {
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
    
    ladder <- api_ladder(api_key, league = match_data$match$leaguename, competition = match_data$match$competitionname)$ranking$team 
    
    if(!is.null(ladder)) {
      ladder <- ladder %>% separate(`w/d/l`, c("Win","Tie","Loss"))
      
      home_ranking <- filter(ladder, name == home_team$teamname)
      away_ranking <- filter(ladder, name == away_team$teamname)
      
      if(nrow(home_ranking) == 1 & nrow(away_ranking) == 1) {
        competition_standing = glue("\n\n{home_ranking$Win}-{home_ranking$Tie}-{home_ranking$Loss} {placing(home_ranking$rank)} V {placing(away_ranking$rank)} {away_ranking$Win}-{away_ranking$Tie}-{away_ranking$Loss}\n")
      }  
    }
  }
  
  if (home_team$score > away_team$score) {home_team$teamname %<>%  md("**")}
  if (away_team$score > home_team$score) {away_team$teamname %<>%  md("**")}
  
  use_team_emoji <- grepl('REBBL', match_data$match$leaguename) & !redirected
  
  is_clan <- grepl('Clan', match_data$match$leaguename)
  
  glue(
    "{format_teamname(home_team, match_data)} V {format_teamname(away_team, match_data)}
    TV {home_team$value} {id_to_race(home_team$idraces)}{ifelse(use_team_emoji,str_c(' ',REBBL_races(id_to_race(home_team$idraces), is_clan)),'')} V {ifelse(use_team_emoji, str_c(REBBL_races(id_to_race(away_team$idraces), is_clan),' '), '')}{id_to_race(away_team$idraces)} {away_team$value} TV {competition_standing}
    {format_division(match_data)}"
  )
}

format_url <- function(match_data) {
  league <- match_data$match$leaguename
  comp <- match_data$match$competitionname
  uuid <- match_data$uuid
  
  case_when(
    grepl("REBBL", league) ~ glue("https://rebbl.net/rebbl/match/{uuid}") %>% as.character(),
    TRUE ~ glue("http://www.mordrek.com/goblinSpy/web/game.html?mid={uuid}") %>% as.character()
  )
}

format_embed <- function(league_params, match_data, redirected = F) {
  list(
    list(
      title = format_title(match_data$coaches),
      description = format_description(match_data, league_params$ladder, redirected),
      url = URLencode(format_url(match_data)),
      color = league_params$colour,
      fields = format_fields(league_params, match_data)
    )
  )
}


# Post matches to discord ----

#Redirect some clan matches for 
redirect_params <- function(league_params, new_hook) {
  league_params$webhook <- new_hook
  
  league_params
}

post_clan <- function(league_params, match_data) {
  clans <- match_data$match$teams %>% map_chr("teamname") %>% str_replace_all("(\\[.+?\\])(.*)","\\1") %>% toupper()
  
  for (clan in clans) {
    if (clan %in% names(clan_hooks)) {
      post_match(redirect_params(league_params, clan_hooks[[clan]]), match_data, check_clans = F, check_race = F)
    }
  } 
}

post_race <- function(league_params, match_data) {
  races <- match_data$match$teams %>% map_int("idraces") %>% map_chr(id_to_race) %>% unique()
  
  for (race in races) {
    if (race %in% names(race_hooks)) {
      post_match(redirect_params(league_params, race_hooks[[race]]), match_data, check_clans = F, check_race = F)
    }
  }
}

post_match <- function(league_params, match_data, times = 0, check_clans = T, check_race = T) {
  #started == finished are admin decided games, mvps = 0|2 means conceded game
  if(pluck(match_data, "match", "started") == pluck(match_data, "match", "finished") | pluck(match_data, "match", "teams", 1, "mvp") != 1) return(NULL)
  
  #if clan league, see if it needs a redirect as well
  if (check_clans & str_detect(match_data$match$leaguename, "(?i)REBBL Clan")) {
    post_clan(league_params, match_data)
  }
  
  if (check_race & any(str_detect(str_to_lower(match_data$match$leaguename), c("big o", "gman", "rel", "rebbl clan","off season")))) {
    post_race(league_params, match_data)
  }
  
  response <- httr::RETRY("POST",
                          url = league_params$webhook,
                          body = list(
                            username = str_trunc(league_params$username, 32, side="right", ellipsis = ""),
                            avatar_url = league_params$avatar,
                            embeds = format_embed(league_params, match_data, redirected = !check_race)
                          ),
                          encode = "json",
                          times = 20,
                          pause_min = 5
  )
  
  if (response$status_code == 429) { #rate limited
    wait_time <- httr::content(response)$retry_after
    print(glue("Rate limited, pausing for {wait_time} seconds."))
    Sys.sleep(wait_time)
  }
  
  if (response$status_code %in% c(204,400)) { #log it and pause for a sec
    Sys.sleep(1)
  }
  
  print(glue::glue("{lubridate::now()}\t{match_data$uuid}\t{match_data$match$leaguename}\t{match_data$match$competitionname}\t{match_data$match$coaches[[1]]$coachname}\t{match_data$match$coaches[[2]]$coachname}\tResponse code:{response$status_code}"))
  response
}

post_matches <- function(league_params, matches, ...) {
  map(matches, ~post_match(league_params, ., ...))
}
