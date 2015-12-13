require("rvest")                                                            
require("magrittr")


key <- "6B962D40-03BA-11E5-BC31-9A51842CA48B" # set key for API requests

# sys.sleep(runif(1,0,2))- used to add pause between steps

get_dennys_info <- function(file.dest, key, lat, long, radius, limit){
#####################################################################
# This function collects the XML response to an API about the location of
# Denny's in the US within a certain distance (radius) of a given point,
# specified by its latitude and longitude
#
# Args
#   file.dest:  where the resulting XML will be stored
#   key:  the key for the API, must be current
#   lat:  the latitude of the center of the search
#   long:  the longitude of the center of the search
#   limit:  a limit on the number of results, cannot exceed 1000
#
# Output
#   A file containing XML in a specified location (file.dest)
#####################################################################
  url <-  paste0( # paste together components to get valid API request
    "https://hosted.where2getit.com/dennys/responsive/ajax?&xml_request=",
    "<request>",
    "<appkey>",key , # insert current key
    '</appkey><formdata id="locatorsearch">',
    "<dataview>store_default</dataview>",
    "<limit>",
    limit, # set limit on number of results
    "</limit>",
    "<order>rank,_distance</order>",
    "<geolocs><geoloc><addressline></addressline>",
    "<longitude>",
    long, # set longitude of center of search
    "</longitude>",
    "<latitude>",
    lat, # set latitude of center of search
    "</latitude>",
    "<country>US", # only returns results in US
    "</country></geoloc></geolocs><stateonly>1</stateonly>",
    "<searchradius>",
    radius, # set radius of search
    "</searchradius></formdata></request>")
  download.file(url, destfile = file.dest, method = "wget") # make API request
}
search.centers <- read.csv("dennys_coords.csv", header = FALSE)
# Important:  in the .csv file, the format is long, lat, radius



gatherData <- function(row.index){
  lat <- search.centers[row.index, 2] # lat is second item in each row
  long <- search.centers[row.index, 1] # long is the first
  radius <- search.centers[row.index, 3] # radius is third
  limit <- 1000 # no reason to adjust this, leave at max
  file.name <- paste0("data/dennys/",row.index,".xml") # see get_dennys.R
  get_dennys_info(file.name, key, lat, long, radius, limit)
}
dir.create("data/dennys/", recursive = TRUE, showWarnings = FALSE)
invisible(lapply(1:nrow(search.centers), gatherData))
