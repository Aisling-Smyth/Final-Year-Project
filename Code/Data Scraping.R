library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Create Data Frame
Movies <- data.frame(Rank = double(),
                     Year = double(),
                     Title = character(),
                     Description = character(),
                     Runtime = double(),
                     Genre = character(),
                     Rating = double(),
                     AgeRatingUS = character(),
                     Metascore = double(),
                     Votes = double(),
                     GrossEarning = double(),
                     Directors = character(),
                     Actors = character(),
                     ID = character(),
                     Link = character(),
                     ImageLink = character())

link = "https://www.imdb.com/search/title/?title_type=feature&genres=musical&view=simple%22%22"
page = read_html(link)
number_of_movies <- (page %>% html_nodes(".desc") %>% html_text())[1] %>% 
  str_extract_all(., "(?<= of).+(?= titles)") %>%
  str_trim("both") %>% parse_number()
number_of_pages <- ceiling(number_of_movies/50)

scrape_movie <- function(Movies, n)
{
  movie <- movie_content[n] 
  
  movie_rank <- movie %>% html_nodes(".text-primary") %>% 
    html_text() 
  
  movie_title <- movie %>% html_nodes(".lister-item-header a") %>% 
    html_text() 
  
  movie_year <- (movie %>% html_nodes(".lister-item-header span") %>% 
                   html_text())[2] %>% gsub("[()]", "", .) %>% parse_number()
  
  movie_age <- movie %>% html_nodes(".text-muted .certificate") %>%
    html_text()
  
  desc <- movie %>% html_nodes('.ratings-bar+ .text-muted') %>% 
    html_text() %>% gsub("\n","",.)
  movie_desc <- ifelse(length(desc)!=0, desc, 
                       toString((movie %>% 
                                   html_nodes(".text-muted+ p"))[1] %>% 
                                  html_text() %>% gsub("\n","",.)))
  
  movie_runtime <- movie %>% html_nodes('.text-muted .runtime') %>% 
    html_text() %>% gsub(" min","",.) 
  
  movie_genre <- movie %>% html_nodes(".genre") %>% 
    html_text() %>% gsub("\n","",.) %>% gsub(" ","",.) 
  
  movie_rating <- movie %>% html_nodes(".ratings-imdb-rating strong") %>% 
    html_text() 
  
  movie_votes <- movie %>% html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% 
    html_text() %>% gsub(",","",.)
  
  directors <- toString(movie %>% html_nodes(".text-muted+ p") %>% 
                          html_text() %>% gsub("\n","",.) %>% 
                          str_extract_all(., "(?<=:).+(?=  \\|)") %>%
                          str_trim("right"))
  movie_directors <- ifelse(length(desc)!=0, directors, toString((movie %>% html_nodes(".text-muted+ p"))[2] %>% 
                                                                   html_text() %>% gsub("\n","",.) %>% 
                                                                   str_extract_all(., "(?<=:).+(?=  \\|)") %>%
                                                                   str_trim("right")))
  
  actors <- toString(movie %>% html_nodes(".text-muted+ p") %>% 
                       html_text() %>% gsub("\n","",.) %>% 
                       str_extract_all(., "(?<=Stars:).+") %>%
                       str_trim("right"))
  movie_actors <- ifelse(length(desc)!=0, actors, toString((movie %>% html_nodes(".text-muted+ p"))[2] %>% 
                                                             html_text() %>% gsub("\n","",.) %>% 
                                                             str_extract_all(., "(?<=Stars:).+") %>%
                                                             str_trim("right")))
  
  movie_score <- movie %>% html_nodes('.metascore') %>% 
    html_text() %>% gsub(" ","",.)  
  
  movie_gross <- movie %>% html_nodes('.ghost~ .text-muted+ span') %>% 
    html_text() %>% gsub("M","",.) %>% substring(.,2,6)  
  
  movie_id <- (movie %>% html_nodes('.lister-item-header a') %>% 
                 html_attr("href")  %>% 
                 str_extract_all(., "(?<=title/).+(?=/)"))[[1]] 
  
  movie_link <- paste("https://www.imdb.com/title/",
                      movie_id,
                      "/?ref_=adv_li_i", sep = "")
  
  movie_image <- (page %>% html_nodes("img"))[n+2] %>%
    html_attr("loadlate")
  
  Movies <- Movies %>% 
    add_row(Rank = ifelse(length(movie_rank) !=0, 
                          as.double(movie_rank), 
                          as.double("NA")), 
            Year = ifelse(length(movie_year) !=0, 
                          as.double(movie_year), 
                          as.double("NA")), 
            Title = ifelse(length(movie_title) !=0, 
                           as.character(movie_title), 
                           as.character("NA")),
            Description = ifelse(length(movie_desc) != 0, 
                                 as.character(movie_desc), 
                                 as.character("NA")), 
            Runtime = ifelse(length(movie_runtime) !=0, 
                             as.double(movie_runtime), 
                             as.double("NA")),
            Genre = ifelse(length(movie_genre) !=0, 
                           as.character(movie_genre), 
                           as.character("NA")), 
            Rating = ifelse(length(movie_rating) !=0, 
                            as.double(movie_rating), 
                            as.double("NA")),
            AgeRatingUS = ifelse(length(movie_age) !=0, 
                                 as.character(movie_age), 
                                 as.character("NA")),
            Metascore = ifelse(length(movie_score) !=0, 
                               as.double(movie_score), 
                               as.double("NA")), 
            Votes = ifelse(length(movie_votes) !=0, 
                           as.double(movie_votes), 
                           as.double("NA")),
            GrossEarning = ifelse(length(movie_gross) !=0, 
                                  as.double(movie_gross), 
                                  as.double("NA")),
            Directors = ifelse(length(movie_directors) !=0, 
                               as.character(movie_directors), 
                               as.character("NA")), 
            Actors = ifelse(length(movie_actors) !=0, 
                            as.character(movie_actors), 
                            as.character("NA")),
            ID = as.character(movie_id),
            Link = as.character(movie_link),
            ImageLink = as.character(movie_image))
}

# naming convention changes after movie 1000 (and I can't figure it out), 
# otherwise, would've used 1:number_of_pages
for(j in 1:200) 
  {
    link = paste("https://www.imdb.com/search/title/?title_type=feature&genres=musical&start=",
    1 + (j-1)*50, "&ref_=adv_nxt", sep = "")

    page = read_html(link)
    movie_content = page %>% html_nodes(".lister-item-content")
    
    for(i in 1:length(movie_content)){Movies <- scrape_movie(Movies, i)}
  
}

write.csv(Movies, file = "C:/UL/FYP/FYP/Movies.csv")
