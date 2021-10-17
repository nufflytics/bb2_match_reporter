library(tidyverse)

league_filter <- regex("Greenhorn|Playoffs|REL|Gman|Big O", ignore_case = T)

player_information <- tribble(
  ~race,     ~type,             ~TV, ~normal_skills, ~base_skills,
  "Nurgle", "Pestigor",         80, "GSM", "Horns|Nurgles Rot|Regeneration",
  "Nurgle", "Nurgle Warrior",   110,"GSM", "Disturbing Presence|Foul Appearance|Nurgles Rot|Regeneration",
  "Nurgle", "Beast of Nurgle",  140,"S",   "Loner|Disturbing Presence|Foul Appearance|Mighty Blow|Nurgles Rot|Really Stupid|Regeneration|Tentacles",
  "Chaos",  "Beastman",         60, "GSM", "Horns",
  "Chaos",  "Warrior",          100,"GSM", "",
  "Chaos",  "Minotaur",         150,"SM", "Loner|Frenzy|Horns|Mighty Blow|Thick Skull|Wild Animal",
  "Chaos Dwarf", "Blocker",     70, "GS", "Block|Tackle|Thick Skull",
  "Chaos Dwarf", "Bull Centaur",130,"GS", "Sprint|Sure Feet|Thick Skull",
  "Chaos Dwarf", "Minotaur",    150,"S", "Loner|Frenzy|Horns|Mighty Blow|Thick Skull|Wild Animal"
)

skill_values <- c(none = 0, normal = 20, double = 30, `+MA`= 30, `+AV` = 30, `+AG` = 40, `+ST` = 50)

skill_table <- list(
    'G' = c('Block','Dauntless','Dirty Player','Fend','Frenzy','Kick','Kick Off Return','Pass Block','Pro','Shadowing','Strip Ball','Sure Hands','Tackle','Wrestle'),
    'A' = c('Catch','Diving Catch','Diving Tackle','Dodge','Jump Up','Leap','Side Step','Sneaky Git','Sprint','Sure Feet'),
    'S' = c('Break Tackle','Grab','Guard','Juggernaut','Mighty Blow','Multiple Block','Piling On','Stand Firm','Strong Arm','Thick Skull'),
    'P' = c('Accurate','Dump Off','Hail Mary Pass','Leader','Nerves of Steel','Pass','Safe Throw'),
    'M' = c('Big Hand','Claw','Disturbing Presence','Extra Arms','Foul Appearance','Horns','Prehensile Tail','Tentacles','Two Heads','Very Long Legs')
)

skill_to_category <- function(skill) {
  if (stringr::str_starts(skill, "\\+") | skill == "") {return(skill)}
  
  purrr::keep(skill_table, ~any(. == skill)) %>% names()
}

category_to_type <- function(skill, normal_skills) {
  if (stringr::str_starts(skill, "\\+")) {return(skill)}
  if (skill == "") {return("none")}
  
  if (str_detect(normal_skills, skill)) {
    return("normal")
  } else {
    return("double")
  }
}

calc_tv <- function(this_race, this_type, this_skills) {

  this_piece <- player_information %>% filter(race == this_race, type == this_type)
  
  base_tv <- this_piece$TV
  
  if (is.na(this_skills)) {
    this_skills <- ""
  }
  
  if (this_piece$base_skills == "") {
    this_piece$base_skills <- "Nothing"
  }
  
  earned_skills <- str_remove_all(this_skills, fixed(this_piece$base_skills)) %>% 
    str_remove("^\\|") %>% 
    str_split("\\|") %>% 
    .[[1]] %>% 
    map_chr(skill_to_category) %>% 
    map_chr(category_to_type, this_piece$normal_skills)
  
  added_value <- sum(skill_values[earned_skills])
  
  base_tv + added_value
}

calc_score <- function(tv, inj) {
  multiplier <- c()
}

data <- read_csv("data/chaos_death_tally.csv") %>% 
  filter(str_detect(league, league_filter)) %>% 
  unique() %>% 
  rowwise() %>% 
  mutate(TV = calc_tv(race, type, skills)) %>% 
  mutate(score = calc_score(TV, injury))
  
