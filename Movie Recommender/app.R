#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(arules)
library(shinythemes)
load("MOVIERECSpring2022.RData")

Movies <- POPULARITY
#TitleSelection <- head(POPULARITY[order(POPULARITY$percentSeen),],500)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cyborg"),

    # Application title
    titlePanel('"Hidden Gem" Movie Recommendation Engine'),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "selected_movies", 
                      label="Select Movies You Like" ,
                      choices=unique(POPULARITY[4]),
                      multiple = TRUE,
                      selected = c(
                        "Longest Yard, The (2005)", "Jumanji: Welcome to the Jungle (2017)", "Toy Story (1995)", "Jumani: Welcome to the Jungle (2017)", "Deadpool (2016)", "Zombieland (2009)"
                      )
          ),
          sliderInput("number_of_recs",
                      "Number of recommendations:",
                      min = 1,
                      max = 100,
                      value = 10
                      ),
          sliderInput(inputId = "min_confidence",
                      label = "Select a Minimum Level of Confindence",
                      min = 0,
                      max = 1,
                      value = 0.5
                      ),
          numericInput(inputId = "max_popularity",
                      label = "Enter a Maximum Popularity (0-30)",
                      min = 0,
                      max = 30,
                      value = 3
                      ),
          submitButton(
                       text = "Click to Update Selections"
                       )
                    ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$table <- DT::renderDataTable({
    req(input$selected_movies)

    # REC CODE START   ----------------------------------------
    RECS <- NULL
    if (isTruthy(input$selected_movies)) {
      # Rule out too popular movies early on
      too_popular <- POPULARITY$title[which(100 * POPULARITY$percentSeen > input$max_popularity)]
      
      # Keep popular movies that the user input
      too_popular <- setdiff(too_popular, input$selected_movies)
      
      min_support <- 4
      max_time <- 0
      
      RULES <- apriori(
        TRANS,
        parameter = list(
          supp = min_support / length(TRANS),
          conf = input$min_confidence,
          minlen = 2,
          maxtime = max_time
        ),
        appearance = list(
          none = too_popular,
          lhs = input$selected_movies,
          default = "rhs"
        ),
        control = list(
          verbose = FALSE
        )
      )
      
      if (length(RULES) > 0) {
        RULES <- RULES[!is.redundant(RULES)]
        RULES <- RULES[is.significant(RULES, TRANS)]
        
        RULESDF <- DATAFRAME(RULES, itemSep = " + ", setStart = "", setEnd = "")
        names(RULESDF)[1:2] <- c("BasedOn", "title")
        
        # Remove recs that the user gave as input
        RULESDF <- RULESDF[!(RULESDF$title %in% input$selected_movies), ]
        if (nrow(RULESDF) > 0) {
          RECS <- aggregate(confidence ~ title, data = RULESDF, FUN = max)
          
          RECS <- merge(RECS, POPULARITY, by = "title")
          
          RECS$item_id <- NULL
          RECS$countSeen <- NULL
          RECS$Year <- NULL
          names(RECS) <- c("Movie", "Confidence", "PercentSeen", "imdbRating")
          
          # Order the recommendations by confidence
          RECS <- RECS[order(RECS$Confidence, decreasing = TRUE), ]
          RECS <- head(RECS, input$number_of_recs)
          
          # Take out confusing row names
          row.names(RECS) <- NULL
          
          RECS$Confidence <- round(RECS$Confidence * 100, 2)
          RECS$PercentSeen <- round(RECS$PercentSeen * 100, 2)
        }
      }
    }
    
    if (is.null(RECS)) {
      RECS <- data.frame(
        Error = "No recommendations with these parameters.  Add more movies, decrease confidence, or increase popularity!"
      )
    }
    
    RECS
    # REC CODE END     ----------------------------------------
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
