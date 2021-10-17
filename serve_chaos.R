#!/usr/bin/env Rscript

library(plumber)

p <- plumber::plumb("chaos_api.R")

p$run(port = 4567)
