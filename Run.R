# Install necessary packages
necessary_packages <- c("cluster",
                        "dplyr",
                        "grid",
                        "jpeg",
                        "owmr",
                        "proxy",
                        "readr",
                        "recommenderlab",
                        "reshape2",
                        "rvest",
                        "shiny", 
                        "shinyjs",
                        "shinythemes",
                        "stringr",
                        "tibble",
                        "tidyr",
                        "tidyverse",
                        "vegan",
                        "vroom")
not_installed <- necessary_packages[!(necessary_packages %in% 
                                        installed.packages()[ , "Package"])]
if(length(not_installed))
  install.packages(not_installed)

source("Set Up Movie Genres.R")
source("Setup.R")
source("Subset Movies.R")
source("Recommend Movies.R")
source("Find Similar Movies.R")
source("ui.R")
source("server.R")

runApp()