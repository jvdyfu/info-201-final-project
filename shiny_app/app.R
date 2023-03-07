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
                  tabPanel("Gender Bar Plot",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("genderInput", "Choose Genders to Display:",
                                           choices = c("Male", "Female", "Unknown"),
                                           selected = "Male",
                                           multiple = TRUE),
                               selectInput("colorInput", "Choose Bar Color:",
                                           choices = c("Blue", "Red", "Green"),
                                           selected = "Blue")),
                             mainPanel(plotOutput("precinct")),
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
  
  
  ## this is for Vivian's tab
  filteredDataOne <- reactive({
    force %>%
      filter(Subject_Gender %in% input$genderInput)
  })
  barColor <- reactive({
    switch(input$colorInput,
           "Blue" = "cornflowerblue",
           "Red" = "red3",
           "Green" = "forestgreen")
  })
  output$precinct <- renderPlot({ ##for vivian's plot
    ggplot(filteredDataOne(), aes(x = Subject_Gender)) +
      geom_bar(fill = barColor()) +
      labs(x = "Gender", y = "Count", title = "Gender Distribution") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
