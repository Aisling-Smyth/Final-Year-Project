library(shiny)
library(shinyjs)
library(shinythemes)
library(vroom)
library(proxy)
library(recommenderlab)
library(reshape2)
library(tidyr)
library(dplyr)
library(readr)
library(tibble)
library(owmr)
library(tidyverse)
library(cluster)
library(stringr)
library(grid)
library("jpeg")
library(vegan)

link = "https://github.com/Aisling-Smyth/Final-Year-Project/blob/main/Data/Movies.csv" 
movies <- vroom(link,
                locale = locale(encoding = "WINDOWS-1252")) # é or similar

movies$Rank = as.integer(movies$Rank)
movies$Year = as.integer(movies$Year)
movies$Runtime = as.integer(movies$Runtime)
movies$Metascore = as.integer(movies$Metascore)
movies$Votes = as.integer(movies$Votes)

genre_list <- sort(unique(separate_rows(as.data.frame(movies$Genre),1,sep = ",")[["movies$Genre"]])) 
genre_list <- genre_list[!genre_list == "Musical"]
actor_list = sort(unique(separate_rows(as.data.frame(movies$Actors),1,sep = ", ")[["movies$Actors"]]))
director_list = sort(unique(separate_rows(as.data.frame(movies$Directors),1,sep = ", ")[["movies$Directors"]]))

UL_logo_src = "https://www.ilovelimerick.ie/wp-content/uploads/2019/10/18D7FE00-2D89-4331-99B6-CB01CC5A4880.jpg"

genres <- add_movies_genre(movies, genre_list) %>% select(Action:Western)
dissimilarity <- as.data.frame(as.matrix(vegdist(genres, method = "jaccard")))
