library(shiny)
library(leaflet)
source("prediction.R")
list_clusters = c("Geographic Location" = "location", 
                  "Offense" = "offense", 
                  "Defense" = "defense", 
                  "Offense and Defense" = "total")


shinyUI(
  fluidPage(theme = "bootstrap.css",
                  
    h3("College Football Match Predictor", align = "center"),
    
    br(),
                  
    fluidRow(
      # choose team 1 and team 2 to predict the result
      column(4, offset = 2, align = "center",
        selectInput("team1", "Team 1", choices = row.names(Offense))
      ),

      column(4, align = "center",
        selectInput("team2", "Team 2", choices = row.names(Offense))
      )
    ),
    
    br(),
    
    fluidRow(
      # show the score for team 1 
      column(4, offset = 2, align = "center",
        wellPanel(
          textOutput("team1"),
          tags$head(tags$style("#team1{
                               font-size: 20px;
                               font-style: bold;}")),
          textOutput("team_1_score"), 
          tags$head(tags$style("#team_1_score{color: white;
                               font-size: 32px;
                               font-style: bold;}"))
        )
      ),
      # show the score for team 2
      column(4, align = "center",
        wellPanel(
          textOutput("team2"),
          tags$head(tags$style("#team2{
                               font-size: 20px;
                               font-style: bold;}")),
          textOutput("team_2_score"),
          tags$head(tags$style("#team_2_score{color: white;
                               font-size: 32px;
                               font-style: bold;}"))
        )
      )
    ),
    
    br(),

    fluidRow(
      # show the winning team
      column(6, offset = 3, align = "center",
        wellPanel(
          textOutput("winning_team"),
          tags$head(tags$style("#winning_team{color: white;
                               font-size: 28px;
                               font-style: bold;}"))
        )
      )
    ),
    
    br(),
    hr(),
    
    h3("Design your own Conference Clustering", align = "center"),
    
    br(),
    # allow users to choose clustering criteria
    fluidRow(
      column(4, offset = 4, align = "center",
             selectInput("cluster_crit", "Cluster by:", list_clusters)
      )
    ),
    
    br(),
    fluidRow(
      # plot the current conference
      column(5, offset = 1, align = "center",
        wellPanel(
          h5("Current Conference Locations", align = "center"),
          leafletOutput("map1", width=450, height=350)
        ),checkboxInput("legend1", "Show legend", TRUE)

      ),
      
      column(5, align = "center",
             wellPanel(
               # clustering map using user selected criteria
               h5("Conference by User Criteria", align = "center"),
               leafletOutput("map2", width=450, height=350)
             )
      )
    ),
    
    br()
  )
)


