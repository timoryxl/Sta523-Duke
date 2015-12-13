library(rvest)
library(magrittr)
library(XML)
library(stringr)
library(ggmap)


if(!file.exists("data/fb/")){
   dir.create("data/fb/", recursive = T, showWarnings = F)
}

base_url <- "http://espn.go.com"
page <- "/college-football/statistics"


if(!file.exists("data/fb/listtings.html")){
   download.file(paste(base_url, page, sep = ""), 
              destfile = "data/fb/listtings.html")
}


# Scrape the urls, however, due to unorganized naming rules, we have to scrape them separatelt
file = read_html("data/fb/listtings.html")
hotel_url1 = html_nodes(file, ".mod-no-footer:nth-child(4) td:nth-child(1) a") %>%
             html_attr("href")
hotel_url1 = c(paste0(base_url, hotel_url1[1:5]),hotel_url1[8])
hotel_url2 = html_nodes(file, ".mod-no-footer+ .mod-no-footer td:nth-child(2) a") %>%
             html_attr("href")
hotel_url2 = c(hotel_url2[1:3], paste0(base_url, hotel_url2[4]), hotel_url2[6])


# We need to specify the rows we want to skip in downs, efficiency and other(norm) tables
skip_down <- c(1,13,14,25,26,37,38,49,50,61,62,73,74,85,86,97,98,109,110,121,122,133,134,145,146)
skip_effi <- c(22,43,64,85,106,127)
skip_norm <- c(12,23,34,45,56,67,78,89,100,111,122,133)


# Because some pages have the same names, we need to scrape them separately and 
# rename them in a reasonable way
for(i in 1:length(hotel_url1)){
  if(i == 5){
    table <- readHTMLTable(hotel_url1[i], header=T, stringsAsFactors=F, skip.rows = skip_down)[[1]]
    
    # add prefix to make the names more easy to use
    names(table)[3:6] <- paste0("firstdowns_", names(table)[3:6])
    names(table)[7:9] <- paste0("thirddowns_", names(table)[7:9])
    names(table)[10:12] <- paste0("fourthdowns_", names(table)[10:12])
    names(table)[13:14] <- paste0("penalties_", names(table)[13:14])
  }else if(i == 6){
    table <- readHTMLTable(hotel_url1[i], header=T, stringsAsFactors=F)[[1]]
    names(table) <- paste(basename(hotel_url1[i]), table[1,], sep = "_")
    table <- table[-c(1,skip_effi),]
  }else{ 
    table <- readHTMLTable(hotel_url1[i], header=T, stringsAsFactors=F, skip.rows = skip_norm)[[1]]
    names(table) <- paste(basename(hotel_url1[i]), names(table), sep = "_")
  }
  if(i == 1){
    Table <- table
  }else{
    Table <- cbind(Table, table)
  }
}


# Rename some of the columns due to duplicated names
Table <- Table[,-c(which(Table[1,]==1),length(Table)-2,length(Table)-1)]
Table$efficiency_TEAM <- unlist(strsplit(Table$efficiency_TEAM,","))[seq(1,255,by=2)]
table1 <- Table[,1:9]
table2 <- Table[,10:22]
table3 <- Table[,23:29]
table4 <- Table[,30:36]
table5 <- Table[,37:49]
table6 <- Table[,50:52]
names(table1)[1] <- "Team"
names(table2)[1] <- "Team"
names(table3)[1] <- "Team"
names(table4)[1] <- "Team"
names(table5)[1] <- "Team"
names(table6)[1] <- "Team"


# Rename some of the universities to make all the tables have the same naming rule
table6[which(table6[,1] %in% setdiff(table6[,1], table1[,1])),1] <- 
  c("Ohio State","Mississippi State","Florida State","Louisiana Tech",
    "Georgia Southern","California","Northern Illinois","Appalachian State",
    "Virginia Tech","Pittsburgh","South Florida","Washington State",
    "Connecticut","North Carolina","Central Michigan","Virginia","Southern Mississippi",
    "East Carolina","Western Kentucky","Florida Atlantic","Texas San Antonio",
    "San JosÃ© State","Florida Intl","Western Michigan","Middle Tennessee",
    "Louisiana Monroe","Massachusetts","Louisiana Lafayette","New Mexico State",
    "Eastern Michigan")

tb1 <- merge(table1, table2, by = "Team", all = T) %>%
  merge(., table3, by = "Team", all = T) %>%
  merge(., table4, by = "Team", all = T) %>%
  merge(., table5, by = "Team", all = T) %>%
  merge(., table6, by = "Team", all = T)



# Repeat for defense data
for(i in 1:length(hotel_url2)){
  if(i == 4){
    table <- readHTMLTable(hotel_url2[i], header=T, stringsAsFactors=F, skip.rows = skip_down)[[1]]
    names(table)[3:5] <- paste0("tackles_", names(table)[3:5])
    names(table)[6:7] <- paste0("sacks_", names(table)[6:7])
    names(table)[8:12] <- paste0("interceptions_", names(table)[8:12])
    names(table)[13] <- paste0("fumbless_", names(table)[13])
  }else if(i == 5){
    table <- readHTMLTable(hotel_url2[i], header=T, stringsAsFactors=F)[[1]]
    names(table) <- paste(basename(hotel_url2[i]), table[1,], sep = "_")
    table <- table[-c(1,skip_effi),]
  }else if(i == 1){
    table <- readHTMLTable(hotel_url2[i], header=T, stringsAsFactors=F, skip.rows = skip_norm)[[1]]
    names(table) <- paste("totaldefense_", names(table), sep = "_")
  }else if(i == 3){
    table <- readHTMLTable(hotel_url2[i], header=T, stringsAsFactors=F, skip.rows = skip_norm)[[1]]
    names(table) <- paste("rushingdefense_", names(table), sep = "_")
  }else{ 
    table <- readHTMLTable(hotel_url2[i], header=T, stringsAsFactors=F, skip.rows = skip_norm)[[1]]
    names(table) <- paste(basename(hotel_url2[i]), names(table), sep = "_")
  }
  if(i == 1){
    Table <- table
  }else{
    Table <- cbind(Table, table)
  }
}

Table <- Table[,-c(which(Table[1,]==1),length(Table)-3,length(Table)-1)]
Table$efficiency_TEAM <- unlist(strsplit(Table$efficiency_TEAM,","))[seq(1,255,by=2)]
table1 <- Table[,1:9]
table2 <- Table[,10:22]
table3 <- Table[,23:29]
table4 <- Table[,30:41]
table5 <- Table[,42:44]

names(table1)[1] <- "Team"
names(table2)[1] <- "Team"
names(table3)[1] <- "Team"
names(table4)[1] <- "Team"
names(table5)[1] <- "Team"

table5[which(table5[,1] %in% setdiff(table5[,1], table1[,1])),1] <- 
  c("Ohio State","Mississippi State","Florida State","Louisiana Tech",
    "Georgia Southern","California","Northern Illinois","Appalachian State",
    "Virginia Tech","Pittsburgh","South Florida","Washington State",
    "Connecticut","North Carolina","Central Michigan","Virginia","Southern Mississippi",
    "East Carolina","Western Kentucky","Florida Atlantic","Texas San Antonio",
    "San JosÃ© State","Florida Intl","Western Michigan","Middle Tennessee",
    "Louisiana Monroe","Massachusetts","Louisiana Lafayette","New Mexico State",
    "Eastern Michigan")

tb2 <- merge(table1, table2, by = "Team", all = T) %>%
  merge(., table3, by = "Team", all = T) %>%
  merge(., table4, by = "Team", all = T) %>%
  merge(., table5, by = "Team", all = T) 


# Scrape the wiki data
url = "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_FBS_football_programs"
d = read_html(url)

data_table_html = d %>% html_nodes("table") %>% html_table()
Wiki_team = as.data.frame(data_table_html[[1]])
Wiki_team = Wiki_team[ ,c(1,3,4,5)]

# Rename and clean
Wiki_team[28,2] = "Storrs"
Wiki_team[58,2] = "Amherst"
Wiki_team[60,2] = "Coral Gables"
Wiki_team[72,2] = "Las Vegas"

Wiki_team[40,3] = "Hawaii"

Team_name = apply(as.matrix(Wiki_team[,1]), 1, function(x) strsplit(x,split=" !")[[1]][1])
Wiki_team[,1] = Team_name

#Combine city and state abbreviation for geocode
State_Ab = apply(Wiki_team, 1, function(x) state.abb[match(x[3],state.name)])
Wiki_team = cbind(Wiki_team, State_Ab)
City_State =apply(Wiki_team, 1, function(x) paste(x[2],x[5], sep=","))
Wiki_team = cbind(Wiki_team, City_State)
Wiki_team = Wiki_team[,-5]


#Geocode for lattitude and longitude
long = 0
lat = 0
#Lat/Long#
for (i in 1:nrow(Wiki_team)){
  geo_obj = geocode(as.character(Wiki_team[i,5]))
  long[i] = as.numeric(geo_obj[[1]])
  lat[i] = as.numeric(geo_obj[[2]])
}

#Save to dataframe
Wiki_team$Longtitude = long
Wiki_team$Latiutude = lat

Wiki_team[which(!(Wiki_team$Team %in% tb1$Team)),1] <- c("BYU", "Fresno State", "UCLA", "UCF", "Florida Intl",
                                                          "Hawaii", "LSU", "Louisiana Lafayette", "Louisiana Monroe",
                                                          "Ole Miss", "UNLV", "NC State", "San JosÃ© State", "USC", 
                                                          "SMU", "Southern Mississippi", "TCU", "UTEP", "Texas San Antonio")
dat_1 <- merge(Wiki_team, tb1, by = "Team")
dat_2 <- merge(Wiki_team, tb2, by = "Team")


# Save the data
Wiki_team <- Wiki_team[match(dat_1$Team, Wiki_team$Team),] #reorder rows
write.csv(dat_1, file = "Scraping/Offense.csv")
write.csv(dat_2, file = "Scraping/Defense.csv")
save(Wiki_team, file = "Scraping/Wiki_team.Rdata")

