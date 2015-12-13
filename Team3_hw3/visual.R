# download and install packages needed
listOfPackages <- c("lubridate", "dplyr", "ggplot2","RgoogleMaps","ggmap")
NewPackages <- listOfPackages[!(listOfPackages %in% installed.packages()[,"Package"])]
if(length(NewPackages)>0) {install.packages(NewPackages,repos="http://cran.rstudio.com/")}

# libaray packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(RgoogleMaps)
library(ggmap)

load("data/analysis_data.Rdata")

# count the number of cases of each complaint type
num.cases<-rep(0,length(unique(data.to.save1$Complaint.Type)))
names(num.cases)<-unique(data.to.save1$Complaint.Type)
for (i in unique(data.to.save1$Complaint.Type)){
  num.cases[i]=sum(data.to.save1$Complaint.Type==i)
}
num.cases[which(num.cases==max(num.cases))]
num.cases[order(num.cases)]

# turns out that the most frequently received type of complaint is heating
# leave only the cases about heating
heating = data.to.save.1[which(data.to.save.1$Complaint.Type=="HEATING"),c("y","x","Created.Date","Borough.y")]

# turn character into date, then grab the year   
heating$Date<-as.Date(heating$Created.Date,"%m/%d/%Y")
heating$Year<-as.numeric(format(heating$Date,'%Y'))

Boroughs <- as.factor(unique(heating$Borough.y))
Years <- unique(heating$Year)
count_heating<-summarise(group_by(heating,Borough.y),n())
count_heating.by_year<-summarise(group_by(heating,Borough.y,Year),n())

# only leave the data with non-missing Borough
heating <- heating[!is.na(heating$Borough.y),]

# plot the heatmap of cases on top of new york city map by year
new_york <- get_map(location = "new york city", zoom = 11, color = "bw",
                   source = "osm")
NYMap <- ggmap(new_york, base_layer = ggplot(aes(x = x, y = y),
                                                 data = heating))
NYMap + stat_density2d(aes(x = x, y = y, fill = ..level..),
                       bins = 5, geom = "polygon",
                       data = heating) +
  scale_fill_gradient(low = "gray", high = "red") +
  facet_wrap(~ Year)





