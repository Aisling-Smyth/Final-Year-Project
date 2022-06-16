similar_movies <- function(title, movies, genre_list, dissimilarity){
  
  index = movies %>% 
    subset(Title == toString(title)) %>% 
    select("Rank") %>% pull()
  
  # Subsetting matrix to be the 5 they rated as rows
  Dissimilarity = dissimilarity %>% select(index) %>% pull()
  df <- data.frame(Dissimilarity)
  row.names(df) <- movies$Rank # to keep track of which movie's which

  recommendations = data.frame(Rank = integer(),
                               Year = integer(),
                               Title = character(),
                               Genre = character(),
                               Rating = double(),
                               Votes = integer(),
                               Metascore = integer(),
                               GrossEarning = double())

  # Rearrange so most similar (0) on top
  df <- df %>%
    rownames_to_column('Rank') %>%
    arrange(Dissimilarity) %>%
    column_to_rownames('Rank')

  # Finding which movies are most similar and finding their information
  indeces <- as.numeric(row.names(df))
  similar_movies <- movies %>%
    select(Rank, Year, Title, Genre, Rating, Votes, Metascore, GrossEarning) %>%
    slice(indeces) %>%
    cbind(Similarity = 1 - df$Dissimilarity) %>% # now 1 = same
    subset(Year < 2022)  # no ones yet to be released (otherwise no info on them)

  similar_movies$WeightedScore = (similar_movies$Similarity)*
    ((similar_movies$Rating*similar_movies$Votes/round(mean(movies$Votes, na.rm = TRUE))) +
       similar_movies$Metascore/round(mean(movies$Metascore, na.rm = TRUE)) +
       similar_movies$GrossEarning/round(mean(movies$GrossEarning, na.rm = TRUE)))

  recommendations <- similar_movies %>%
    subset(Title != toString(title)) %>% # take out the rated movies
    arrange(desc(WeightedScore)) %>% # highest rated on top
    slice(1:5) %>% # take the top 5
    mutate(Rank = 1:5)
  
  return(recommendations)
}
