movie_subset <- function(movies, year1, year2, age, 
                         genres, actors, directors) 
{
  # Error proofing for no Genres/actors/directors selected and subsetting
  if(is.null(genres) && is.null(actors) && is.null(directors)){
    movies <- movies
  }
  else{
    if(!is.null(genres)){
      movies1 <- movies %>%
        subset(movies$Genre %>% grepl(paste(genres, collapse="|"), .))
    }
    else{
      movies1 <- NULL
    }
    
    if(!is.null(actors)){
      movies2 <- movies %>%
        subset(movies$Actors %>% grepl(paste(actors, collapse="|"), .))
    }
    else{
      movies2 <- NULL
    }
    
    if(!is.null(directors)){
      movies3 <- movies %>%
        subset(movies$Directors %>% grepl(paste(directors, collapse="|"), .))
    }
    else{
      movies3 <- NULL
    }
    
    movies <- rbind(movies1, movies2, movies3)
  }
  
  
  # Year filter
  movies <- movies %>% filter(Year >= year1, Year <= year2)
  
  # Age Filter
  movies <- movies %>% rename(AgeRating = AgeRatingUS)
  movies$AgeRating[is.na(movies$AgeRating)] <- "PG"
  movies$AgeRatingUS <- movies$AgeRating
  # movies %>% group_by(AgeRatingUS) %>% count() # check available levels
  movies <- movies %>% 
    mutate(AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "PG", 0),
           AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "Approved", 0),
           AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "G", 0),
           AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "R", 18),
           AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "(Banned)", 18),
           AgeRatingUS = replace(AgeRatingUS, AgeRatingUS == "PG-13", 13)) %>%
    mutate(AgeRatingUS = parse_number(AgeRatingUS))
  
  movies <- movies %>% subset(.,  AgeRatingUS <= age)
  movies <- movies %>% subset(select = -c(AgeRatingUS))
  
  return(movies)
}
