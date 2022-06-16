server <- function(input, output, session) 
{
  # Search Movies Tab
  updateSelectizeInput(session, 'Movie_chosen', choices = movies$Title, server = TRUE)
  
  row = reactive({
    movies[movies$Title == input$Movie_chosen, ]  %>% slice(1)
  })
  
  output$Movie_chosen <- renderUI({
    fluidRow(h3(row() %>% select("Title")))
  })
  
  output$movie_desc <- renderUI({
    fluidRow(row() %>% select("Description"))
  })
  
  output$Image <- renderUI({
    poster_path <- row() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$Imdb_Link<-renderUI({
    a("IMDB Page",
      href = row() %>% select("Link"))
  })
  
  similar <- reactive(
    {
      similar_movies(input$Movie_chosen, movies, genre_list, dissimilarity)
    }
  )
  
  # output$table3 <- renderTable({similar()})
  
  # 1
  similar_title1 = reactive(
    {
      similar() %>%
        subset(Rank == 1) %>%
        select("Title")
    })
  
  similar_row1 = reactive({
    movies[movies$Title == toString(similar_title1()), ] %>% slice(1)
  })
  
  output$similar_title_1 <- renderUI({
    fluidRow(h3(paste("1",
                      similar_row1() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$similar_desc1 <- renderUI({
    fluidRow(similar_row1() %>% select("Description"))
  })
  
  output$similar_img1 <- renderUI({
    poster_path <- similar_row1() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$similar_imdb1<-renderUI({
    a("IMDB Page",
      href = similar_row1() %>% select("Link")
    )
  })
  
  # 2
  similar_title2 = reactive(
    {
      similar() %>%
        subset(Rank == 2) %>%
        select("Title")
    })
  
  similar_row2 = reactive({movies[movies$Title == toString(similar_title2()), ] %>% slice(1)})
  
  output$similar_title_2 <- renderUI({
    fluidRow(h3(paste("2",
                      similar_row2() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$similar_desc2 <- renderUI({
    fluidRow(similar_row2() %>% select("Description"))
  })
  
  output$similar_img2 <- renderUI({
    poster_path <- similar_row2() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$similar_imdb2<-renderUI({
    a("IMDB Page",
      href = similar_row2() %>% select("Link")
    )
  })
  
  # 3
  similar_title3 = reactive(
    {
      similar() %>%
        subset(Rank == 3) %>%
        select("Title")
    })
  
  similar_row3 = reactive({movies[movies$Title == toString(similar_title3()), ] %>% slice(1)})
  
  output$similar_title_3 <- renderUI({
    fluidRow(h3(paste("3",
                      similar_row3() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$similar_desc3 <- renderUI({
    fluidRow(similar_row3() %>% select("Description"))
  })
  
  output$similar_img3 <- renderUI({
    poster_path <- similar_row3() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$similar_imdb3<-renderUI({
    a("IMDB Page",
      href = similar_row3() %>% select("Link")
    )
  })
  
  # 4
  similar_title4 = reactive(
    {
      similar() %>%
        subset(Rank == 4) %>%
        select("Title")
    })
  
  similar_row4 = reactive({movies[movies$Title == toString(similar_title4()), ] %>% slice(1)})
  
  output$similar_title_4 <- renderUI({
    fluidRow(h3(paste("4",
                      similar_row4() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$similar_desc4 <- renderUI({
    fluidRow(similar_row4() %>% select("Description"))
  })
  
  output$similar_img4 <- renderUI({
    poster_path <- similar_row4() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$similar_imdb4<-renderUI({
    a("IMDB Page",
      href = similar_row4() %>% select("Link")
    )
  })
  
  # 5
  similar_title5 = reactive(
    {
      similar() %>%
        subset(Rank == 5) %>%
        select("Title")})
  
  similar_row5 = reactive({movies[movies$Title == toString(similar_title5()), ] %>% slice(1)})
  
  output$similar_title_5 <- renderUI({
    fluidRow(h3(paste("5",
                      similar_row5() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$similar_desc5 <- renderUI({
    fluidRow(similar_row5() %>% select("Description"))
  })
  
  output$similar_img5 <- renderUI({
    poster_path <- similar_row5() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$similar_imdb5<-renderUI({
    a("IMDB Page",
      href = similar_row5() %>% select("Link")
    )
  })
  
  # Recommendation Tab
  updateSelectizeInput(session, 'actors', choices = actor_list, server = TRUE)
  updateSelectizeInput(session, 'directors', choices = director_list, server = TRUE)
  
  # Subset to top 25 titles
  ratable_movies <- movies[1:25, ]
  ratable_titles <- ratable_movies[sample(nrow(ratable_movies)), "Title"]
  
  # Get each title separately 
  for(n in 1:5){
    assign(paste("rate_movie", toString(n), sep = "_"),
           ratable_titles[n, ] %>% pull()
    )
  }
  
  # Output each title for user to rank
  output$movie1 <- renderText({rate_movie_1})
  output$movie2 <- renderText({rate_movie_2})
  output$movie3 <- renderText({rate_movie_3})
  output$movie4 <- renderText({rate_movie_4})
  output$movie5 <- renderText({rate_movie_5})
  
  redo <- observeEvent(
    input$reloadButton, 
    session$reload()
  )
  
  recommendations <- eventReactive(
    input$updateButton,
    {
      Title <- c(rate_movie_1, rate_movie_2,
                 rate_movie_3, rate_movie_4, rate_movie_5)
      Rating <- c(input$rating1, input$rating2,
                  input$rating3, input$rating4, input$rating5)
      user_ratings <- data.frame(Title, Rating) %>%
        arrange(desc(Rating))
      
      if(sum(user_ratings %>% select(Rating)) == 0){
        session$reload()
      }
      else{
        poss_movies <- movie_subset(movies, input$year[1],
                                    input$year[2], input$age,
                                    c(input$genre1,input$genre2,
                                      input$genre3,input$genre4,
                                      input$genre5),
                                    input$actors, input$directors)
        
        recommender_system(movies,
                           poss_movies,
                           user_ratings,
                           genre_list,
                           dissimilarity)
      }
    }
  )
  
  # output$table2 <- renderTable({recommendations()})
  
  # 1
  title1 = eventReactive(
    input$updateButton,
    {
      recommendations() %>%
        subset(Rank == 1) %>%
        select("Title")
    })
  
  row1 = reactive({
    movies[movies$Title == toString(title1()), ] %>% slice(1)
  })
  
  output$recommended_title1 <- renderUI({
    fluidRow(h3(paste("1",
                      row1() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$recommended_desc1 <- renderUI({
    fluidRow(row1() %>% select("Description"))
  })
  
  output$recommended_img1 <- renderUI({
    poster_path <- row1() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$recommended_imdb1<-renderUI({
    a("IMDB Page",
      href = row1() %>% select("Link")
    )
  })
  
  # 2
  title2 = eventReactive(
    input$updateButton,
    {
      recommendations() %>%
        subset(Rank == 2) %>%
        select("Title")
    })
  
  row2 = reactive({movies[movies$Title == toString(title2()), ] %>% slice(1)})
  
  output$recommended_title2 <- renderUI({
    fluidRow(h3(paste("2",
                      row2() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$recommended_desc2 <- renderUI({
    fluidRow(row2() %>% select("Description"))
  })
  
  output$recommended_img2 <- renderUI({
    poster_path <- row2() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$recommended_imdb2<-renderUI({
    a("IMDB Page",
      href = row2() %>% select("Link")
    )
  })
  
  # 3
  title3 = eventReactive(
    input$updateButton,
    {
      recommendations() %>%
        subset(Rank == 3) %>%
        select("Title")
    })
  
  row3 = reactive({movies[movies$Title == toString(title3()), ] %>% slice(1)})
  
  output$recommended_title3 <- renderUI({
    fluidRow(h3(paste("3",
                      row3() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$recommended_desc3 <- renderUI({
    fluidRow(row3() %>% select("Description"))
  })
  
  output$recommended_img3 <- renderUI({
    poster_path <- row3() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$recommended_imdb3<-renderUI({
    a("IMDB Page",
      href = row3() %>% select("Link")
    )
  })
  
  # 4
  title4 = eventReactive(
    input$updateButton,
    {
      recommendations() %>%
        subset(Rank == 4) %>%
        select("Title")
    })
  
  row4 = reactive({movies[movies$Title == toString(title4()), ] %>% slice(1)})
  
  output$recommended_title4 <- renderUI({
    fluidRow(h3(paste("4",
                      row4() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$recommended_desc4 <- renderUI({
    fluidRow(row4() %>% select("Description"))
  })
  
  output$recommended_img4 <- renderUI({
    poster_path <- row4() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$recommended_imdb4<-renderUI({
    a("IMDB Page",
      href = row4() %>% select("Link")
    )
  })
  
  # 5
  title5 = eventReactive(
    input$updateButton,
    {
      recommendations() %>%
        subset(Rank == 5) %>%
        select("Title")})
  
  row5 = reactive({movies[movies$Title == toString(title5()), ] %>% slice(1)})
  
  output$recommended_title5 <- renderUI({
    fluidRow(h3(paste("5",
                      row5() %>% select("Title"),
                      sep = ". ")))
  })
  
  output$recommended_desc5 <- renderUI({
    fluidRow(row5() %>% select("Description"))
  })
  
  output$recommended_img5 <- renderUI({
    poster_path <- row5() %>% select("ImageLink")
    img(src = poster_path, height = "200")
  })
  
  output$recommended_imdb5<-renderUI({
    a("IMDB Page",
      href = row5() %>% select("Link")
    )
  })
}
