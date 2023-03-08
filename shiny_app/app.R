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
                titlePanel("Seattle Police Department: Use Of Force Policy"),
                tabsetPanel(
                  tabPanel("Introduction to Title 8",
                           mainPanel(fluidRow(
                             column(8,
                            h2("Purpose:"),
                             h5("Police brutality has risen around the country particularly with the targeting of marginalized communities (Black, Indigenous, Persons of Color, LGBTQIA+)
                                which has began conversations surrounding (de)funding, (de)militarization, and the role of police in society as a whole. The Use of Force policy was implemented
                                on 4/15/2021 in an attempt to ensure constant evaluation, impartial policing, and building accountability with the communities they serve. Wherever our peers
                                stand on this debate, we believe this discourse and our opinions develop as new data uncovers patterns within policing."),
                             
                             h2("Source:"),
                             h5("The data came the City of Seattle’s open data portal (data.seattle.gov) which contains data published by city departments. The department we chose to
                                investigate was the Seattle Police Department, particularly SPD’s Use Of Force policy which is broken down in 4 levels:"),
                             h5("Level 1 - Restraint/Transitory pain"),
                             h5("Level 2 - Bodily harm"),
                             h5("Level 3 - Bodily risk"),
                             h5("Level 3 - Officer Involved Shooting (OIS)"),
                             h3("Relevant information:"),
                             h5("This dataset contained", nrow(force), "documented Use Of Force incidents in the City of Seattle."),
                             h5("The dataset reports incidents under eight different racial categories:", textOutput("race")),
                             h5("The dataset also breaks down the victims upon gender categories:", textOutput("gender"))),
                             column(4,
                                    HTML("<br>"),
                                  h5("The dataset operates under these areas:", textOutput("area")),
                             img(src = "seattleprecinct.jpeg", height = 400, width = 300))
                           )
                  )),
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
