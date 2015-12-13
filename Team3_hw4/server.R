################################ 
### Shiny App Server
################################ 

### Load required packages
library(ggmap)
library(jsonlite)
library(magrittr)
library(ggmap)
library(maps)
library(maptools)
library(MazamaSpatialUtils)
key = "72dd839e601dd06f1e48e16ec40604c9"

weather = c("clear-day", "clear-night", "rain", "snow", "sleet", "wind", "fog", "cloudy", "partly-cloudy-day", "partly-cloudy-night")


### Create the shiny server
shinyServer(
  function(input, output, session){
    
    datalist = reactive(
      { 
        loc = geocode(input$Cities, source = "google")
        url = paste0("https://api.forecast.io/forecast/", key, "/", loc[[2]], ",", loc[[1]])
        dat = fromJSON(url)$hourly
        dat
      }
    )
    
    summary = reactive(
      {
        datalist()$summary
      }
    )
    
    data = reactive(
      {
        dat = datalist()$data
        dat$time = as.POSIXct(dat$time, origin = "1970-01-01")
        dat
      }
    )
    
    output$temperature_plot = renderPlot(
      {
        t <- data()$time %>% 
          str_replace("2015-12", "Dec") %>% 
          str_replace(":00:00", "") %>% 
          str_replace("-", "/")
        par(mar = c(8,10,4,10))
        if(input$Attributes %in% c("precipIntensity", "precipProbability", 
                                   "apparentTemperature", "dewPoint", "humidity", "windSpeed", 
                                   "windBearing", "visibility", "cloudCover", "pressure", "ozone")){
          plot(data()$time, data()$temperature, xaxt = "n", xlab = "", 
               ylab = "temperature(F)", main = paste0("temperature(F) and ", input$Attributes, " hourly predictions"),
               col = "red", type = "b")
          par(new = T)
          plot(data()$time, data()[,input$Attributes], ylab = " ", xlab = " ", axes = F, 
               col = "blue", type = "b")
          axis(1, labels = t, at = data()$time, las = 2)
          mtext("hour", side = 1, line = 6)
          mtext(input$Attributes, side = 4, line = 5) 
          axis(4, seq(range(data()[,input$Attributes])[1], range(data()[,input$Attributes])[2], length.out = 5), las = 1)
          legend("topright", c("temperature", input$Attributes),lty=c(1,1),
                 lwd=c(2.5,2.5), col=c("red", "blue"), cex=1.2)
        }else{
          plot(data()$time, data()$temperature, xaxt = "n", xlab = "", 
               ylab = "temperature(F)", main = "temperature(F) hourly predictions",
               col = "red", type = "b")
          axis(1, labels = t, at = data()$time, las = 2)
          mtext("hour", side=1, line = 6)
        }
      }
    )
    
    
    output$other_plot = renderPlot(
      {
        t <- data()$time %>% 
          str_replace("2015-12", "Dec") %>% 
          str_replace(":00:00", "") %>% 
          str_replace("-", "/")
        if(input$Attributes %in% c("precipIntensity", "precipProbability", 
                                   "apparentTemperature", "dewPoint", "humidity", "windSpeed", 
                                   "windBearing", "visibility", "cloudCover", "pressure", "ozone")){
          par(mar = c(8,5,3,4))
          plot(data()$time, data()[,input$Attributes], xaxt = "n", xlab = "", 
               ylab = input$Attributes, main = paste0(input$Attributes, " hourly predictions"),
               col = "blue", type = "b")
          axis(1, labels = t, at = data()$time, las = 2)
          mtext("hour", side=1, line = 6)
        }else if(input$Attributes == "icon"){
          par(mar = c(8,8,3,4))
          Response <- factor(data()$icon, levels = weather, labels = 1:10)
          plot(data()$time, Response, ylim = c(1,10), xaxt = "n", yaxt = "n",xlab = "", ylab = input$Attributes)
          axis(1, labels = t, at = data()$time, las = 2)
          mtext("hour", side=1, line = 6)
          axis(2, at = 1:10, labels = weather, las = 1)
        }else if(input$Attributes == "precipType"){
          pre <- data()[,input$Attributes] 
          pre[is.na(pre)] <- "NA"
          barplot(table(pre))
        }else{
          barplot(table(data()[,input$Attributes]))
        }
      }
    )
    
    output$summary = renderText({
      summary()
    })
    
    output$location = renderText({
      loc = geocode(input$Cities, source = "google")
      paste0("Longitude is ", loc[[1]], " and latitude is ", loc[[2]])
    })
    
    output$country = renderText(
      {
        loc = geocode(input$Cities, source = "google")
        getCountryName(loc[[1]], loc[[2]])
      }
    )
    
    output$Worldmap = renderPlot(
      {
        ll.visited <- geocode(input$Cities, source = "google")
        visit.x <- ll.visited$lon
        visit.y <- ll.visited$lat
        map("world", fill=TRUE, col="white", bg="grey", ylim=c(-60, 90), mar=c(0,0,0,0))
        points(visit.x,visit.y, col="red", pch = 8, cex = 2)
      }
    )
    
  }
)
