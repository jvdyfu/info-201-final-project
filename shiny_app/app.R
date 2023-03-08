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

useofforce <- force %>% 
  select(-ID, -Incident_Num, -Occured_date_time, -Precinct, -Sector, -Beat,
         -Officer_ID, -Subject_ID, -Subject_Race, -Subject_Gender) %>%
  unlist() %>%
  setdiff(force)

# I added my introduction slide to this document
ui <- fluidPage(
                titlePanel("Seattle Police Department: Use Of Force Policy"),
                tabsetPanel(
                  tabPanel("Introduction",
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
                           fluidRow(column(8, h4("This gender bar plot attempts to answer the 
                                                 question of whether or not there is a correlation
                                                 between gender identity and the number of police cases
                                                 where force was used. "))),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("genderInput", "Choose Genders to Display:",
                                           choices = c("Male", "Female", "Unknown", "Non-binary",
                                                       "Other", "Transgender Male", "Transgender Female"),
                                           selected = "Male",
                                           multiple = TRUE),
                               selectInput("colorInput", "Choose Bar Color:",
                                           choices = c("Blue", "Red", "Green"),
                                           selected = "Blue")),
                             mainPanel(plotOutput("precinct")),
                           )
                  ),
                  
                  tabPanel("Race Bar Plot",
                           fluidRow(column(8, h4("This race bar plot attempts to answer the 
                                                 question of whether or not there is a correlation
                                                 between race and the number of police cases
                                                 where force was used. We also answer the
                                                 question of if there's a correlation with the
                                                 level of force used (incident type)."))),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(
                                 "uof",
                                 "Which level of the use of force would you like to see?",
                                 choices = useofforce,
                                 selected = "Level 1 - Use of Force")
                             ),
                             mainPanel = plotOutput("racexuof")
                           )
                  ),
            
                  tabPanel("Location Scatter Plot",
                           fluidRow(column(8, h4("This location scatter plot attempts to answer the 
                                                 question of whether or not there is a correlation
                                                 between the location (precinct and sector) of the police 
                                                 cases where force was used has a correlation with the
                                                 number of cases. We also can answer the question
                                                 of if there's a correlation with the level of 
                                                 force used (incident type)."))),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(
                                 "loc",
                                 "Which level of the use of force would you like to see?",
                                 choices = useofforce,
                                 selected = "Level 1 - Use of Force")
                             ),
                             
                             mainPanel = plotOutput("locationxuof")
                    
                           )
                  ),
                  
                  tabPanel("Conclusion Takeaways",
                           fluidRow(column(8, 
                            h3("Description of Notable Insights or Patterns"),
                            h4("In terms of gender identity, a notable insight is that the most predominant
                               gender of the victims are male while the lowest identity were non-binaries
                               and transgender. There is surprisingly more cases where the victims are transgender
                               females than transgender males."),
                            DT::dataTableOutput("genderTable"),
                            h4("In terms of race, a notable insight is that white people remain the predominant
                               race of victims across all levels of the use of force. Black people follow closely
                               in second though where as every other race have way less victims in comparision
                               to the top two."),
                            DT::dataTableOutput("raceTable"),
                            h4("In terms of locations, the highest amount of cases with level 1 use of force
                               resides in West King. The highest amount of cases with level 2
                               resides in East Edward. The amount of cases for level 3 are all relatively the same.
                               Lastly, the highest amount of cases for level 3 OIS resides in North Union."),
                            DT::dataTableOutput("locationTable"),
                            h3("The Broader Implications of Insight"),
                            h4("In terms of gender, it seems that males are the majority of the victims,
                               which suggests that police does not target women and other identities.
                               Within races, although the race is predominantly white, we have to take 
                               in account that the largest race group is still white people. Since the number of 
                               cases where the victims are black are follow closely as the second most group,
                               this suggests that signs of police brutality reside in targetting black communities. 
                               For the locations, it seems that West King, East Edward, and North Union has the highest
                               amount of cases for each type of use of force except for level 3. These locations
                               have some of the highest populations of marginalized communities living in them, suggesting
                               that there is evidence of police brutality targeting marginalized communities."),
                            h3("Data Quality"),
                            h4("We found our dataset to be of reasonable quality due to it being a reliable source
                               and clear naming conventions. We do think that it could give biased results. For example,
                               although white people were the race that were the most predominant in victims, this 
                               can be manipulated to ignore the evident targetting of marginalized communities
                               if we didn't take in consideration that there is a higher population of white people
                               than any other race. This could potentially harm people of color as it dismisses the 
                               problem that people of color are often victims of force by the police due to racism."),
                            h3("Future Ideas"),
                            h4("Some ideas on how to advance the project include being able to compare the number of
                               victims by race to the population of that race within Seattle to better answer our 
                               main question. We could also create a tab panel to be able to view cases by time to 
                               see whether or not the time of day a case occurs is correlated with gender or race of 
                               the victim. This could answer questions such as: Are people of color more likely to be
                               victims of police force at night compared to the morning? What about in comparision
                               to white people?"),
                            )),
                           
                           )
                  
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
  
  output$racexuof <- renderPlot({ #daniel's race x use of force plot
    
    
    subset <- reactive({ #filter to react with input
      force %>% 
        filter(Incident_Type %in% input$uof)
    })
    
    output$racexuof <- renderPlot({ #plot
      subset() %>% 
        group_by(Subject_Race, Incident_Type) %>%
        summarize(count = n()) %>%
        ggplot(aes(x = Subject_Race, y = count, fill = Incident_Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Race and Use of Force",
             x = "Race",
             y = "Number of Cases where Force was Used ",
             fill = "Incident Type"
             )
    })

  })
  
  output$locationxuof <- renderPlot({
    
    
    subset <- reactive({ #filter to react with input
      force %>% 
        filter(Incident_Type %in% input$loc)
    })
    
    output$locationxuof <- renderPlot({ #plot
      subset() %>% 
        filter(!is.na(Precinct), !is.na(Sector)) %>% 
        group_by(Precinct, Sector, Incident_Type) %>% 
        summarize(count = n()) %>% 
        mutate(loc = paste(Precinct, " ", Sector, sep="")) %>% 
        ggplot(aes(x = loc, y = count, fill = Incident_Type, col = Incident_Type, size = count)) +
        geom_point(stat = "identity", position = "dodge") +
        labs(title = "Location and Use of Force",
             x = "Location",
             y = "Number of Cases where Force was Used",
             fill = "Incident Type",
             col = "Incident Type",
             size = "Number of Cases where Force was Used")
    })
    
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
  
  genderCount <- useOfForceData %>% 
    group_by(Subject_Gender) %>% 
    summarize(count = n())
  
  output$genderTable = DT::renderDataTable({
    genderCount
  })
  
  raceCount <- useOfForceData %>% 
    group_by(Subject_Race, Incident_Type) %>% 
    summarize(count = n())
  
  output$raceTable = DT::renderDataTable({
    raceCount
  })
  
  locationCount <- useOfForceData %>% 
    filter(!is.na(Precinct), !is.na(Sector)) %>% 
    group_by(Precinct, Sector, Incident_Type) %>% 
    summarize(count = n()) 
  
  output$locationTable = DT::renderDataTable({
    locationCount
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
