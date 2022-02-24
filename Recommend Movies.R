recommender_system <- function(movies, poss_movies, user_ratings, 
                               genre_list, dissimilarity){
  
  # User rated 5 out of top 25 movies - finding which
  movies_options <- movies[1:25, ]
  rated <- user_ratings %>% select("Title") %>% pull()
  rated_movies <- movies_options[movies_options$Title %in% rated, 1] %>% pull()
  names <- movies_options[rated_movies, 3] %>% pull()
  
  # Subsetting matrix to be the 5 they rated as rows
  dissimilarity_user_unordered <- dissimilarity %>% select(rated_movies)
  colnames(dissimilarity_user_unordered) <- names # so I can keep track of which is which
  
  # Separating out each row by (rated) movie
  names_order <- user_ratings %>% select("Title")
  for(n in 1:5){
    assign(paste("movie", n, sep = "_"),
           dissimilarity_user_unordered[ , toString(names_order[n, 1])])
  }
  rm(dissimilarity_user_unordered) # takes up a lot of room
  
  # Creating new dissimilarity, columns are 5 rated movies (in order of how highly rated)
  dissimilarity_user <- cbind(movie_1, movie_2, movie_3, movie_4, movie_5)
  row.names(dissimilarity_user) <- movies$Rank
  
  recommendations = data.frame(Rank = integer(),
                               Year = integer(),
                               Title = character(),
                               Genre = character(),
                               Rating = double(),
                               Votes = integer(),
                               Metascore = integer(),
                               GrossEarning = double())#,
  # Similarity = double(),
  # OriginalMovieRating = integer(),
  # WeightedScore = double())
  
  for(n in 1:5){
    # If they haven't seen it don't use it for ratings
    r = user_ratings %>% 
      subset(., user_ratings$Title == names_order[n, 1]) %>% 
      select("Rating")
    if(r == 0)
    {
      dissimilarity_user <- dissimilarity_user[, -c(n)]
      next
    }
    
    # Subset to nth movie
    df <- data.frame(Dissimilarity = dissimilarity_user[, n])
    row.names(df) <- movies$Rank # to keep track of which movie's which
    
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
      subset(Year < 2022) %>% # no ones yet to be released (otherwise no info on them)
      add_column(OriginalMovieRating = r) # what the user rated the one it's similar to
    
    # Unsure how to weight it - went with similariy*rating(this rating/metsacroe/gorssearning over mean of each for now)
    # Most movies have NAs for these though so it doesn't really work - don't know how to work around
    
    similar_movies$WeightedScore = (similar_movies$Similarity*similar_movies$OriginalMovieRating)*
      ((similar_movies$Rating*similar_movies$Votes/round(mean(movies$Votes, na.rm = TRUE))) + 
         similar_movies$Metascore/round(mean(movies$Metascore, na.rm = TRUE)) +
         similar_movies$GrossEarning/round(mean(movies$GrossEarning, na.rm = TRUE)))
    
    recommendations <- rbind(recommendations, similar_movies)
  }
  
  # Subset to include any user constraints
  poss_titles = poss_movies %>% select("Title") %>% pull()
  recommendations <- recommendations %>%
    subset(Title %in% poss_titles)
  
  recommendations <- recommendations %>%
    subset(!Title %in% names_order[, 1]) %>% # take out the rated movies 
    filter(duplicated(Title) == FALSE) %>% # only one instance of each row
    arrange(desc(WeightedScore)) %>% # highest rated on top
    slice(1:5) %>% # take the top 5
    mutate(Rank = 1:5)
  
  rm(dissimilarity_user)
  
  return(recommendations)
}