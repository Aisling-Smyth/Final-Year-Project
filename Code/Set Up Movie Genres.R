add_movies_genre <- function(movies, genre_list){
  
  movies_genre = (data.frame(as.list(genre_list)) %>% 
                    rename_with(~str_remove(., 'X.')) %>%
                    rename_with(~str_remove(., '\\.')))[-1,]
  
  movies_genre <- data.frame("Action" = replicate(nrow(movies), 0),
                             "Adventure" = replicate(nrow(movies), 0),
                             "Animation" = replicate(nrow(movies), 0),
                             "Biography" = replicate(nrow(movies), 0),
                             "Comedy" = replicate(nrow(movies), 0),
                             "Crime" = replicate(nrow(movies), 0),
                             "Drama" = replicate(nrow(movies), 0),
                             "Family" = replicate(nrow(movies), 0),
                             "Fantasy" = replicate(nrow(movies), 0),
                             "History" = replicate(nrow(movies), 0),
                             "Horror" = replicate(nrow(movies), 0),
                             "Music" = replicate(nrow(movies), 0),
                             "Musical" = replicate(nrow(movies), 0),
                             "Mystery" = replicate(nrow(movies), 0),
                             "Reality-TV" = replicate(nrow(movies), 0),
                             "Romance" = replicate(nrow(movies), 0),
                             "Sci-Fi" = replicate(nrow(movies), 0),
                             "Sport" = replicate(nrow(movies), 0),
                             "Thriller" = replicate(nrow(movies), 0),
                             "War" = replicate(nrow(movies), 0),
                             "Western" = replicate(nrow(movies), 0), 
                             check.names=FALSE)
  
  movies_genre <- cbind(movies, movies_genre) 
  
  for(n in 1:nrow(movies)){
    for(j in 1:length(genre_list)){
      if(any(movies$Genre[n] %>% strsplit(",") %>% unlist() == 
             toString(genre_list[j]))) {
        col = which(names(movies_genre) == toString(genre_list[j]))
        movies_genre[n, col] = 1
      }
    }
  }
  
  movies_genre <- movies_genre %>% select(-c("Musical"))
  
  return(movies_genre)
}
