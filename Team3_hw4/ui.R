################################ 
### Shiny App User Interface
################################ 

### Load required package
library(shiny)

city <- c("Amsterdam", "Ankara", "Athens", "Atlantic City", "Baltimore",
          "Bangkok", "Beijing", "Berlin", "Berne",
          "Brussels", "Budapest", "Buenos Aires", "Cairo", 
          "Canberra", "Cape Town", "Chicago", "Cologne",
          "Copenhagen", "Damascus", "Delhi", "Dubai", 
          "Dublin", "Florence", "Genève", "Ha Noi", "Havana", "Helsinki", 
          "Hong Kong", "Honolulu", "Istanbul", "Jakarta", "Jerusalem", 
          "Kansas City", "Kathmandu", "Kuala Lumpur", "Lisbon", 
          "London", "Los Angeles", "Luxembourg", 
          "Madrid", "Manila", "Melbourne", "Mexico City", "Milan", 
          "Montreal", "Moscow", "Mumbai", "Munich", "Nazareth", 
          "New York", "Nice", "Osaka", "Ottawa", "Oslo",
          "Paris", "Philadelphia", "Phnom Penh", "Prague", "Quito", 
          "Reykjavík", "Rio de Janeiro", "San Francisco", "Santiago", 
          "São Paulo", "Shanghai", "Singapore", "Stockholm", "Saint-Petersburg", 
          "Sydney", "Taipei", "Tokyo", "Toronto", "Venice", 
          "Vienna", "Washington", "Zürich")

attributes <- c("summary", "icon", "precipIntensity", "precipProbability", 
                "apparentTemperature", "dewPoint", "humidity", "windSpeed", 
                "windBearing", "visibility", "cloudCover", "pressure", "ozone", "precipType")

shinyUI(
  fluidPage(
    ### Give the panel a title
    titlePanel(
       "Weather forecast"
    ),
    ### Create a sidebar for user inputs
    fluidRow(
      column(5, align = "center",
             h2("Major cities:"),
             selectInput("Cities", label = "Choose a city to display", choices = city),
             hr(),
             selectInput("Attributes", label = "Choose a second quantity you want to display",
                         choices = attributes, selected = "precipIntensity")),
      column(5,
             h3("Summary"),
             h4(textOutput("summary")),
             h3("Location"),
             h4(textOutput("location")),
             h3("Country"),
             h4(textOutput("country"))
      )
    ),
    ### Show results in main panel
    br(),
    hr(),
    fluidRow(
      h2("Results:"),
      plotOutput("temperature_plot")
    ),
      
    fluidRow(
      column(6, offset = 1,
             wellPanel(
               plotOutput("other_plot")
             )
      ),
      column(4,
             wellPanel(
               plotOutput("Worldmap")
             )
      )
    )
)
)