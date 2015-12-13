library(rvest)
library(magrittr)
library(stringr)

files = dir("data/lq/", pattern="*.html", full.names=TRUE) # Create a list of files

# NA vector for latitudes and longitudes
data=rep(NA,length(files)) 
# Renames the individuals entries for compatibility with our for loop below
names(data)=files 

# NA vector for number of floors
floors=rep(NA,length(files)) 
# Rename for compatibility
names(floors)=files 

# NA vector for hotel names
hnames=rep(NA,length(files))
# Rename for compatibility
names(hnames)=files

# NA vector for hotel address
haddress=rep(NA,length(files))
# Rename for compatibility
names(haddress)=files

# NA vector for phone number
hphone=rep(NA,length(files))
# Rename for compatibility
names(hphone)=files

# NA vector for city
hcity=rep(NA,length(files))
# Rename for compatibility
names(hcity)=files

# NA vector for fax number
hfax=rep(NA,length(files))
# Rename for compatibility
names(hfax)=files

for (file in files){
  # read each html file
  html = read_html(file) 
  # extract the minimap, link to img (contains lat and long)
  lat_long = html_nodes(html, ".minimap") %>% html_attr("src") 
  # extract number of floors
  floorno = html_nodes(html, ".hotelFeatureList li:nth-child(1)") 
  # extract hotel name
  hotel_name = html_nodes(html, "h1")
  # extract hotel info
  hotel_info = html_nodes(html, ".hotelDetailsBasicInfoTitle p")
  # extract the latitude and longitude (through regular expressions)
  data[file] = str_match(lat_long,"-?[0-9]{1,2}\\.[0-9]+,-?[0-9]{1,3}\\.[0-9]+") 
  # extract the number of floors
  floors[file] = str_match(floorno,"[0-9][0-9]?") 
  # extract the hotel name
  hnames[file] = str_match(hotel_name,"[A-Za-z]{1}[A-Za-z \\&\\;]{1,}")
  # extract the hotel address
  haddress[file] = str_match(hotel_info,">.*,") %>%
    str_match (.,"[0-9]{1,} [0-9a-zA-z\\-\\. ]{1,}")
  # extract the city name and zip code
  hcity[file] = str_match(hotel_info, "[A-Za-z]{1,}, [A-Za-z ]{1,} [0-9]{5}")
  # extract the phone number
  hphone[file] = str_match(hotel_info,"Phone: [0-9].[0-9]{3}.[0-9]{3}.[0-9]{4}") %>% 
    str_match(., "[0-9].[0-9]{3}.[0-9]{3}.[0-9]{4}")
  # extract the fax number
  hfax[file] = str_match(hotel_info,"Fax: [0-9].[0-9]{3}.[0-9]{3}.[0-9]{4}") %>% 
    str_match(., "[0-9].[0-9]{3}.[0-9]{3}.[0-9]{4}")
}

# Next: split up latitude and longitude
# Zero vector for latitude
lat=rep("0",length(data))
# Zero for longitude
long=rep("0",length(data)) 
# NA vector for state abbreviation of hotel 
state=rep(NA,length(data)) 

for (i in 1:length(data)){
  # Split lat and long, before comma is lat
  lat[i]=str_split(data[i],",")[[1]][1] 
  # After comma is long
  long[i]=str_split(data[i],",")[[1]][2] 
}

for (i in 1:length(hnames)){
  # The symbol "&" is coded differently in html
  # We have to replace the html code with the real symbol
  hnames[i]=str_replace(hnames[i], "&amp;", "&")
}

for (i in 1:length(hcity)){
  # split the city name of the hotel to get the state abbreviation
  state[i]=str_split(str_split(hcity[i],",")[[1]][2]," ")[[1]][2]
}

# turn our vectors into numeric vectors
lat=as.numeric(lat) 
long=as.numeric(long)
floors=as.numeric(floors)

# Combine all the vectors into a data frame
lq_data=data.frame(hnames,haddress,hcity,state,hphone,hfax,lat,long,floors) 
# Name the data frame according to the entries
colnames(lq_data)=c("Hotel Name","Address","City","state","Phone","Fax","latitude","longitude","Floors") 
rownames(lq_data)=NULL

# Check that our data frame looks like it is supposed to
summary(lq_data) 
head(lq_data)
tail(lq_data)

# save lq_data under the data/ folder
save(lq_data, file="data/lq_data.Rdata")