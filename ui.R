ui <- navbarPage(
  id = "navbar",
  theme = shinytheme("slate"),
  title = div(img(src = UL_logo_src, 
                  width = "110px", height = "49px", 
                  style="margin-top: -14px; margin-right:-14px;margin-left:-14px", 
                  height = 60)),
  
  tabPanel(tags$b('About'),
           
           fluidRow(
             h3('This project has been developed by Aisling Smyth 18233511 as part of her FYP.',
                style="text-align: center"),
             br(),
             br()
           ),
           
           div(
             tags$img(src = UL_logo_src, height=150), 
             style="text-align: center; margin-bottom:10px;"
           ),
           
           fluidRow(br(),
                    br(),
                    column(1),
                    column(6,
                           (h5("This app has multiple capabilities; you can search for information about over 10,000 movies using the 'Search Movies' tab, or a movie can be recommended for you based upon your preferences."))
                    ),
                    column(2, h5("R Code"), 
                           tags$a(href="https://github.com/UL18233511/FYP-Interim-Code",
                                  "Available here")
                    ),
                    column(2, h5("Shiny App"),
                           tags$a(href="https://aisling-smyth.shinyapps.io/interim_report/",
                                  "Available here")
                    )
           )
  ),
  
  tabPanel(tags$b('Search By Movie'),
           fluidRow(column(1),
                    column(8,selectizeInput("Movie_chosen", "Select your movie",
                                            choices = NULL)
                    )),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("Movie_chosen"))),
                             fluidRow(column(7,uiOutput("movie_desc"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("Imdb_Link")))
                      ),
                      column(4, htmlOutput('Image'))
             )
           ),
           fluidRow(h3("Similar Titles You May Like:")),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("similar_title_1"))),
                             fluidRow(column(7,uiOutput("similar_desc1"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("similar_imdb1")))
                      ),
                      column(4, htmlOutput('similar_img1'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("similar_title_2"))),
                             fluidRow(column(7,uiOutput("similar_desc2"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("similar_imdb2")))
                      ),
                      column(4, htmlOutput('similar_img2'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("similar_title_3"))),
                             fluidRow(column(7,uiOutput("similar_desc3"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("similar_imdb3")))
                      ),
                      column(4, htmlOutput('similar_img3'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("similar_title_4"))),
                             fluidRow(column(7,uiOutput("similar_desc4"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("similar_imdb4")))
                      ),
                      column(4, htmlOutput('similar_img4'))
             )
           ), 
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("similar_title_5"))),
                             fluidRow(column(7,uiOutput("similar_desc5"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("similar_imdb5")))
                      ),
                      column(4, htmlOutput('similar_img5'))
             )
           ),
  ),
  
  tabPanel(tags$b('Movie Recommender'),
           fluidRow(h3("Please Rate the Following:"), 
                    h5("If you have not seen a movie, please select 0.")),
           wellPanel(
             fluidRow(column(6, textOutput("movie1"), 
                             style = 'font-size:200%; color:green'),
                      column(6, sliderInput("rating1", 
                                            label = "Please Rate Movie #1",
                                            min = 0,
                                            max = 5,
                                            value = 0,
                                            dragRange = FALSE))
             ),
             fluidRow(column(6, textOutput("movie2"), style = 'font-size:200%'),
                      column(6, sliderInput("rating2", label = "Please Rate Movie #2",
                                            min = 0,
                                            max = 5,
                                            value = 0,
                                            dragRange = FALSE))
             ),
             fluidRow(column(6, textOutput("movie3"), style = 'font-size:200%; color:green'),
                      column(6, sliderInput("rating3", label = "Please Rate Movie #3",
                                            min = 0,
                                            max = 5,
                                            value = 0,
                                            dragRange = FALSE))
             ),
             fluidRow(column(6, textOutput("movie4"), style = 'font-size:200%'),
                      column(6, sliderInput("rating4", label = "Please Rate Movie #4",
                                            min = 0,
                                            max = 5,
                                            value = 0,
                                            dragRange = FALSE))
             ),
             fluidRow(column(6, textOutput("movie5"), style = 'font-size:200%; color:green'),
                      column(6, sliderInput("rating5", label = "Please Rate Movie #5",
                                            min = 0,
                                            max = 5,
                                            value = 0,
                                            dragRange = FALSE))
             ),
             fluidRow(column(1, h3(""))),
             fluidRow(column(12, align = "center", 
                             h3("Haven't seen these movies?"),
                             actionButton("reloadButton", "Refresh Options", 
                                          style='font-size:100%')))),
           
           fluidRow(column(1, h3(""))),
           fluidRow(h3("Select Specific Criteria (Optional)")),
           wellPanel(
             fluidRow(
               column(6, sliderInput("year", label = "Select Movie Year Range",
                                     min = min(movies$Year, na.rm = TRUE) %>%
                                       as.numeric(gsub(",","", .)),
                                     max = max(movies$Year, na.rm = TRUE) %>%
                                       as.numeric(gsub(",","", .)),
                                     value = c(min(movies$Year, na.rm = TRUE) %>%
                                                 as.numeric(gsub(",","", .)),
                                               max(movies$Year, na.rm = TRUE) %>%
                                                 as.numeric(gsub(",","", .))),
                                     sep = "")),
               column(6, sliderInput("age", label = "Select An Age Limit",
                                     min = 0,
                                     max = 100,
                                     value = 100,
                                     dragRange = FALSE))),
             fluidRow(column(1, h3(""))),
             fluidRow(column(6,
                             selectizeInput("actors", "Search Actors:", 
                                            choices = NULL, multiple = TRUE)),
                      column(6,uiOutput("actor_choices")),
                      column(6,selectizeInput("directors", "Search Directors:",
                                              choices = NULL, multiple = TRUE)),
                      column(6,uiOutput("director_choices"))),
             fluidRow(
               column(1),
               column(2,checkboxGroupInput("genre1", " ",
                                           genre_list[1:4])),
               column(2,checkboxGroupInput("genre2", " ",
                                           genre_list[5:8])),
               column(2,checkboxGroupInput("genre3", " ",
                                           genre_list[9:12])),
               column(2,checkboxGroupInput("genre4", " ",
                                           genre_list[13:16])),
               column(2,checkboxGroupInput("genre5", " ",
                                           genre_list[17:20]))
             )),
           fluidRow(column(1, h3(""))),
           fluidRow(column(6, align="center", offset = 3,
                           actionButton("updateButton", "Create Recommendations!", 
                                        style='font-size:200%'))),
           fluidRow(column(1, h3(""))),
           fluidRow(h3("Top Picks For You:")),
           
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("recommended_title1"))),
                             fluidRow(column(7,uiOutput("recommended_desc1"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("recommended_imdb1")))
                      ),
                      column(4, htmlOutput('recommended_img1'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("recommended_title2"))),
                             fluidRow(column(7,uiOutput("recommended_desc2"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("recommended_imdb2")))
                      ),
                      column(4, htmlOutput('recommended_img2'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("recommended_title3"))),
                             fluidRow(column(7,uiOutput("recommended_desc3"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("recommended_imdb3")))
                      ),
                      column(4, htmlOutput('recommended_img3'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("recommended_title4"))),
                             fluidRow(column(7,uiOutput("recommended_desc4"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("recommended_imdb4")))
                      ),
                      column(4, htmlOutput('recommended_img4'))
             )
           ),
           wellPanel(
             fluidRow(column(1),
                      column(7,
                             fluidRow(column(7,uiOutput("recommended_title5"))),
                             fluidRow(column(7,uiOutput("recommended_desc5"))),
                             br(),
                             br(),
                             fluidRow(column(7,uiOutput("recommended_imdb5")))
                      ),
                      column(4, htmlOutput('recommended_img5'))
             )
           )
  )
)
