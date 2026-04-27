#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = FALSE)
script <- sub("--file=", "", args[grep("--file=", args)])
app_dir <- dirname(normalizePath(script))

shiny::runApp(
  app_dir,
  launch.browser = TRUE,
  host           = "127.0.0.1",
  port           = 3838
)
