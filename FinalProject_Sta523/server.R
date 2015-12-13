source("prediction.R")

shinyServer(
  function(input, output, session) 
  {
    observe(
      {
        if(input$team1==input$team2){updateSelectInput(session,"team2", choices = row.names(Offense)[row.names(Offense)!=input$team1])}
      }
    )
  team.score = reactive({
  team1.score = numeric()
  team2.score = numeric()
  team1.offense = Offense[input$team1, ] 
  team1.defense = Defense[input$team1, ]
  team2.offense = Offense[input$team2, ] 
  team2.defense = Defense[input$team2, ]
  #Select relevant variables that have non-0 coefficients for regression model#
  
  team1.rank = team1.offense %>%
    select(CFP.5:AP.6_25)
  
  team1.offense1 = team1.offense %>%
    select(efficiency_OVERALL:rushing_TD)
  
  team2.defense1 = team2.defense %>%
    select(efficiency_DEFENSE, tackles_TOTAL:SpecialTeams.Punting_AVG)
  score1.1 = rbind(intercept = 1, efficiency_DEFENSE=team2.defense$efficiency_DEFENSE, 
                   t(team1.offense1), t(team2.defense1[-1]), t(team1.rank ))
  
  ##Predict number of points that Team 1 will score##
  team1.o = as.numeric(round(t(score1.1) %*% coef(r.defense)))
  
  ##Find how many points team1 will allow#
  team1.defense1 = team1.defense %>%
    select(efficiency_OVERALL:rushingdefense__TD)
  team2.offense1 = team2.offense %>%
    select(efficiency_OFFENSE, firstdowns_RUSH:Returning_Punts_AVG)
  score1.2 = rbind(intercept = 1, efficiency_Offense=team2.offense$efficiency_OFFENSE, 
                   t(team1.defense1), t(team2.offense1[-1]), t(team1.rank ))
  
  ##Predict number of points that Team 1 will allow##
  team1.d = as.numeric(round(t(score1.2) %*% coef(r.offense)))
  
  
  
  ##Find how many points team2 will score##
  team2.rank = team2.offense %>%
    select(CFP.5:AP.6_25)
  
  team2.offense2 = team2.offense %>%
    select(efficiency_OVERALL:rushing_TD)
  
  team1.defense2 = team1.defense %>%
    select(efficiency_DEFENSE, tackles_TOTAL:SpecialTeams.Punting_AVG)
  score2.1 = rbind(intercept = 1, efficiency_DEFENSE=team1.defense$efficiency_DEFENSE, 
                   t(team2.offense2), t(team1.defense2[-1]), t(team2.rank ))
  
  ##Predict number of points that Team 2 will score##
  team2.o = as.numeric(round(t(score2.1) %*% coef(r.defense)))
  
  ##Find how many points team2 will allow#
  team2.defense2 = team2.defense %>%
    select(efficiency_OVERALL:rushingdefense__TD)
  team1.offense2 = team1.offense %>%
    select(efficiency_OFFENSE, firstdowns_RUSH:Returning_Punts_AVG)
  score2.2 = rbind(intercept = 1, efficiency_Offense=team1.offense$efficiency_OFFENSE, 
                   t(team2.defense2), t(team1.offense2[-1]), t(team2.rank ))
  
  ##Predict number of points that Team 2 will allow##
  team2.d = as.numeric(round(t(score2.2) %*% coef(r.offense)))
  
  
  ##Average the number of points Team1 will score and Team2 will allows to find##
  ##Score for Team 1
  team1.score = round(mean(c(team1.o, team2.d)))
  ##Average the number of points Team2 will score and Team1 will allows to find##
  ##Score for Team 2
  team2.score = round(mean(c(team2.o, team1.d)))
  
  # report the winning team based on predicted scores
      if(team1.score>team2.score){
        outcome = paste(input$team1, "Wins!")
      } else { outcome = paste(input$team2, "Wins!")}
  list(team1.score,team2.score,outcome)
})
    output$team1 <- renderText(input$team1) # report selected team 1 name
    output$team2 <- renderText(input$team2) # report selected team 2 name
    output$team_1_score <- renderText({team.score()[[1]]}) # report predicted team 1 score
    output$team_2_score <- renderText({team.score()[[2]]}) # report predicted team 2 score
    output$winning_team <- renderText({team.score()[[3]]}) # report winning team
    
    pal <- colorFactor("RdYlBu", domain = unique(Wiki_team$Current.Conference))
    output$map1 <- renderLeaflet({
      leaflet(data = Wiki_team) %>%
        addTiles() %>% 
        setView(lng = -98, lat = 37.45, zoom = 4) %>% 
        addCircleMarkers(
          color = ~pal(Current.Conference),
          stroke = FALSE, fillOpacity = 0.5,
          popup = rownames(Wiki_team)
          ) # plot the current conference
    })
    observe({
      proxy <- leafletProxy("map1", data = Wiki_team)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      # show the legend of current conference if the users choose to show it
      if (input$legend1) {
        pal <- colorFactor("RdYlBu", domain = unique(Wiki_team$Current.Conference))
        proxy %>%   addLegend("topleft", pal = pal, values = ~Current.Conference,
                              title = "Current Conference",
                              opacity = 1
        )
      }
    })
    
    output$map2 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        setView(lng = -98, lat = 37.45, zoom = 4)
    }) # initialize map2
    observe({
      cluster.geo <- data.frame(cluster = cluster.geo$cluster, 
                                Longitude = Wiki_team$Longitude,
                                Latitude = Wiki_team$Latitude)
      pal1 <- colorFactor("RdYlBu", domain = unique(cluster.geo$cluster))
      proxy <- leafletProxy("map2", data = cluster.geo)
      proxy %>% clearControls()
      if (input$cluster_crit=="location") {
        proxy %>% addCircleMarkers(
          color = ~pal1(cluster),
          stroke = FALSE, fillOpacity = 0.5,
          popup = rownames(cluster.geo)
        )
      } # plot clustering map using location as the criteria
      cluster.O <- data.frame(cluster = cluster.O$cluster, 
                                Longitude = Wiki_team$Longitude,
                                Latitude = Wiki_team$Latitude)
      pal2 <- colorFactor("RdYlBu", domain = unique(cluster.O$cluster))
      proxy <- leafletProxy("map2", data = cluster.O)
      proxy %>% clearControls()
      if (input$cluster_crit=="offense") {
        proxy %>% addCircleMarkers(
          color = ~pal2(cluster),
          stroke = FALSE, fillOpacity = 0.5,
          popup = rownames(cluster.O)
        )
      } # plot clustering map using offense as the criteria
      cluster.D <- data.frame(cluster = cluster.D$cluster, 
                                Longitude = Wiki_team$Longitude,
                                Latitude = Wiki_team$Latitude)
      pal3 <- colorFactor("RdYlBu", domain = unique(cluster.D$cluster))
      proxy <- leafletProxy("map2", data = cluster.D)
      proxy %>% clearControls()
      if (input$cluster_crit=="defense") {
        proxy %>% addCircleMarkers(
          color = ~pal3(cluster),
          stroke = FALSE, fillOpacity = 0.5,
          popup = rownames(cluster.D)
        )
      } # plot clustering map using defense as the criteria
      cluster.total <- data.frame(cluster = cluster.total$cluster, 
                                Longitude = Wiki_team$Longitude,
                                Latitude = Wiki_team$Latitude)
      pal4 <- colorFactor("RdYlBu", domain = unique(cluster.total$cluster))
      proxy <- leafletProxy("map2", data = cluster.total)
      proxy %>% clearControls()
      if (input$cluster_crit=="total") {
        proxy %>% addCircleMarkers(
          color = ~pal4(cluster),
          stroke = FALSE, fillOpacity = 0.5,
          popup = rownames(cluster.total)
        )
      } # plot clustering map using total as the criteria
    })
  }
)
