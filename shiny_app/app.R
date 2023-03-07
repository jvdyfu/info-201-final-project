#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

force <- read_delim("Use_Of_Force.csv")

# I added my introduction slide to this document
ui <- fluidPage(
                titlePanel("Seattle Use Of Force Policing"),
                tabsetPanel(
                  tabPanel("Introduction",
                           mainPanel(
                             h3("There are", nrow(force), "documented Use Of Force incidents in the City of Seattle"),
                             HTML("<br>"),
                             h5("The dataset reports incidents under eight different racial categories:", textOutput("race")),
                             HTML("<br>"),
                             h5("The dataset also breaks down the victims upon gender categories:", textOutput("gender")),
                             HTML("<br>"),
                             h5("The dataset operates under these Precincts/Areas:", textOutput("area")),
                             img(src = "seattleprecinct.jpeg", height = 400, width = 300)
                           )
                  ),
                  tabPanel("Comparison",
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(
                                 "races",
                                 "Input graph title?",
                                 #this is just from my PS
                                 choices = unique(force$Subject_Race))
                             ),
                             mainPanel = plotOutput("racexuof")
                           )
                  ),
                  tabPanel("Table")
                )
)

server <- function(input, output) {
  
  output$race <- renderText({
    race_list <- unique(force$Subject_Race)
    paste(race_list, collapse = ", ")
  })
  
  output$gender <- renderText({
    gender_list <- unique(force$Subject_Gender)
    paste(gender_list, collapse = ", ")
  })
  
  output$area <- renderText({
    precinct_list <- unique(force$Precinct)
    paste(precinct_list, collapse = ", ")
  })
  
  output$racexuof <- renderPlot({
    #this is just a random plot for now
    x <- 5
    y <- 5
    plot(x,y)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
