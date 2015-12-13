library(rvest)
library(magrittr)
library(stringr)

# Directory for the html files
dir.create("data/lq/main/", recursive=TRUE, showWarnings = FALSE)

# Object containing the base url
base_url = "http://www.lq.com" 

# Downloads main file with links to all hotels
download.file("http://www.lq.com/en/findandbook/hotel-listings.html",
              destfile="data/lq/main/listings.html")

# We read this file as html file
files = read_html("data/lq/main/listings.html")
files

# Extract the links to all hotels by combining the base url and the individual links
nodes = html_nodes(files, "#hotelListing .col-sm-12 a") %>%
  html_attr("href") %>% .[!is.na(.)] %>%
  paste0(base_url,.)
nodes

# For loop for downloading all html files of hotels
for (url in nodes){
  # We download all files
  download.file(url, destfile=paste0("data/lq/",basename(url)))
  # 10 second delay so website doesn't block us (download > 1 hour)
  Sys.sleep(10)
}
