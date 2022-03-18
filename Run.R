# Install necessary packages
necessary_packages <- c("shiny", 
                        "shinyjs",
                        "shinythemes",
                        "vroom",
                        "proxy",
                        "recommenderlab",
                        "reshape2",
                        "tidyr",
                        "dplyr",
                        "readr",
                        "tibble",
                        "owmr",
                        "tidyverse",
                        "cluster",
                        "stringr",
                        "grid",
                        "jpeg",
                        "vegan",
                        "rvest",
                        "dplyr",
                        "stringr",
                        "tidyr",
                        "readr",
                        "here")
not_installed <- necessary_packages[!(necessary_packages %in% 
                                        installed.packages()[ , "Package"])]
if(length(not_installed))
  install.packages(not_installed)

# Set working directory to where files are saved
library("here")
setwd(here()) # please ensure that the wd is where these files are saved

source("Set Up Movie Genres.R")
source("Setup.R")
source("Subset Movies.R")
source("Recommend Movies.R")
source("Find Similar Movies.R")
source("ui.R")
source("server.R")

runApp()