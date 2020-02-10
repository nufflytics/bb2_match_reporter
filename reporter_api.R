#!/usr/bin/env Rscript

library(plumber)
source("report.R")
suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(glue))
suppressMessages(library(stringr))
suppressMessages(library(nufflytics))

api_key <- readRDS("data/api.key")
  
# Check redirect data
clan_hooks <- readRDS("data/clan_hooks.rds")
  
race_hooks <- readRDS("data/race_hooks.rds")

# This will post all with rebbl.net team links
league_key = "1siRNzFH3hawaQn4P4c3ukSj23NDwM4hF_hDNZadYOL4"

p <- plumber::plumb("api_post.R")


p$run(port = 4356)


