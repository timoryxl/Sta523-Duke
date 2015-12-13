library(rvest)
library(stringr)

#Offense Total#
#Read url#
url = "http://espn.go.com/college-football/statistics/team/_/stat/total"
d = read_html(url)

#Scrape data as html_table#
data_table_html = d %>% html_nodes("table") %>% html_table()
Offense_Total = as.data.frame(data_table_html[[1]])
#Edit column names to include "Offense"#
colnames(Offense_Total)  = paste("Offense_Total",Offense_Total[1,], sep="_")
#Drop rankings column#
Offense_Total  = Offense_Total[-1,]

#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Offense_Total = Offense_Total[-(del), ]
}


#Defense Total#
url = "http://espn.go.com/college-football/statistics/team/_/stat/total/position/defense/sort/totalYards"
d = read_html(url)

#Scrape data as html_table#
data_table_html = d %>% html_nodes("table") %>% html_table()
Defense_Total = as.data.frame(data_table_html[[1]])
#Edit column names to include "Defense"#
colnames(Defense_Total)  = paste("Defense_Total",Defense_Total[1,], sep="_")
#Drop rankings column#
Defense_Total  = Defense_Total[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Defense_Total = Defense_Total[-(del), ]
}

#Offense Passing#
#Read url#
url = "http://espn.go.com/college-football/statistics/team/_/stat/passing"
d = read_html(url)
#Scrape data as html_table#
data_table_html = d %>% html_nodes("table") %>% html_table()
Offense_Passing = as.data.frame(data_table_html[[1]])
#Edit column names to include "Offense_Passing"#
colnames(Offense_Passing)  = paste("Offense_Passing",Offense_Passing[1,], sep="_")
#Drop rankings column#
Offense_Passing  = Offense_Passing[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Offense_Passing = Offense_Passing[-(del), ]
}

#Defense Passing#
url = "http://espn.go.com/college-football/statistics/team/_/stat/passing/position/defense/sort/passingYards"
d = read_html(url)
#Scrape data as html_table#
data_table_html = d %>% html_nodes("table") %>% html_table()
Defense_Passing = as.data.frame(data_table_html[[1]])
#Edit column names to include "Offense_Passing"#
colnames(Defense_Passing)  = paste("Defense_Passing",Defense_Passing[1,], sep="_")
#Drop rankings column#
Defense_Passing  = Defense_Passing[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Defense_Passing = Defense_Passing[-(del), ]
}

#Offense Rushing#
url = "http://espn.go.com/college-football/statistics/team/_/stat/rushing"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete rankings column#
data_table_html = d %>% html_nodes("table") %>% html_table()
Offense_Rushing = as.data.frame(data_table_html[[1]])
colnames(Offense_Rushing)  = paste("Offense_Rushing",Offense_Rushing[1,], sep="_")
Offense_Rushing  = Offense_Rushing[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Offense_Rushing = Offense_Rushing[-(del), ]
}


#Defense Rushing#
url = "http://espn.go.com/college-football/statistics/team/_/stat/rushing/position/defense/sort/rushingYards"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete rankings column#
data_table_html = d %>% html_nodes("table") %>% html_table()
Defense_Rushing = as.data.frame(data_table_html[[1]])
colnames(Defense_Rushing)  = paste("Defense_Rushing",Defense_Rushing[1,], sep="_")
Defense_Rushing  = Defense_Rushing[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Defense_Rushing = Defense_Rushing[-(del), ]
}

#Downs#
url = "http://espn.go.com/college-football/statistics/team/_/stat/downs"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table(fill=T)
Downs = as.data.frame(data_table_html[[1]])
colnames(Downs) = c(Downs[2,1:2], paste("Downs_FirstDown",Downs[2,3:6], sep="_"), paste("Downs_ThirdDowns",Downs[2,7:9], sep="_"),  
                    paste("Downs_FourthDowns",Downs[2,10:12], sep="_"), paste("Downs_Penalties",Downs[2,13:14], sep="_"))
Downs = Downs[-c(1,2),-ncol(Downs)]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Downs = Downs[-c(del, del+1), ]
}


#Returning#
url = "http://espn.go.com/college-football/statistics/team/_/stat/returning"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table(fill=T)
Returning = as.data.frame(data_table_html[[1]])
colnames(Returning) = c(Returning[2,1:2], paste("Returning_Kickoffs",Returning[2,3:7], sep="_"), 
                        paste("Returning_Punts",Returning[2,8:10], sep="_"))
Returning = Returning[-c(1,2),-c(11:13)]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Returning = Returning[-c(del, del+1), ]
}

#Kicking#
url = "http://espn.go.com/college-football/statistics/team/_/stat/kicking"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table(fill=T)
Kicking = as.data.frame(data_table_html[[1]])
colnames(Kicking) = c(Kicking[2,1:2], paste("Kicking_FieldGoals",Kicking[2,3:11], sep="_"), 
                      paste("Kicking_ExtraPoints",Kicking[2,12:14], sep="_"))
Kicking = Kicking[-c(1,2),-c(15:17)]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Kicking = Kicking[-c(del, del+1), ]
}

#Defense#
url = "http://espn.go.com/college-football/statistics/team/_/stat/defense"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table(fill=T)
Defense = as.data.frame(data_table_html[[1]])
colnames(Defense) = c(Defense[2,1:2], paste("Defense_Tackles",Defense[2,3:5], sep="_"), paste("Defense_Sacks",Defense[2,6:7], sep="_"), 
                      paste("Defense_Interceptions",Defense[2,8:12], sep="_"), paste("Defense_Fumbles",Defense[2,13], sep="_"))
Defense = Defense[-c(1,2),]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Defense = Defense[-c(del, del+1), ]
}

#Receiving
url = "http://espn.go.com/college-football/statistics/team/_/stat/receiving"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table()
Receiving = as.data.frame(data_table_html[[1]])
colnames(Receiving)  = paste("Receiving", Receiving[1,], sep ="_")
Receiving  = Receiving[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Receiving = Receiving[-(del), ]
}

#Punting
url = "http://espn.go.com/college-football/statistics/team/_/stat/punting"
d = read_html(url)
#Scrape data as html_table, edit columns names and delete unused columns and rows#
data_table_html = d %>% html_nodes("table") %>% html_table()
Punting = as.data.frame(data_table_html[[1]])
colnames(Punting)  = paste("Punting",Punting[1,], sep = "_")
Punting  = Punting[-1,]
#Delete certain subsection heading within table#
del=1
for(i in 1:12){
  del = del + 10
  Punting = Punting[-(del), ]
}


#Merging the dataframes

#Sort and delete rank
Defense.s = Defense[order(Defense[,2]),]
Defense.s = Defense.s[,-1]

Defense_Passing.s = Defense_Passing[order(Defense_Passing[,2]),]
Defense_Passing.s = Defense_Passing.s[,-1]

Defense_Rushing.s = Defense_Rushing[order(Defense_Rushing[,2]),]
Defense_Rushing.s = Defense_Rushing.s[,-1]

Defense_Total.s = Defense_Total[order(Defense_Total[,2]),]
Defense_Total.s = Defense_Total.s[,-1]

Downs.s = Downs[order(Downs[,2]),]
Downs.s = Downs.s[,-1]

Kicking.s = Kicking[order(Kicking[,2]),]
Kicking.s = Kicking.s[,-1]

Offense_Total.s = Offense_Total[order(Offense_Total[,2]),]
Offense_Total.s = Offense_Total.s[,-1]

Offense_Rushing.s = Offense_Rushing[order(Offense_Rushing[,2]),]
Offense_Rushing.s = Offense_Rushing.s[,-1]

Offense_Passing.s = Offense_Passing[order(Offense_Passing[,2]),]
Offense_Passing.s = Offense_Passing.s[,-1]

Punting.s = Punting[order(Punting[,2]),]
Punting.s = Punting.s[,-1]

Receiving.s = Receiving[order(Receiving[,2]),]
Receiving.s = Receiving.s[,-1]

Returning.s = Returning[order(Returning[,2]),]
Returning.s = Returning.s[,-1]

#Bind togther all dataframes#
Team_Statistics = cbind(Offense_Total.s ,Defense_Total.s[,-1], Downs.s[,-1], Offense_Passing.s[,-1], 
                        Defense_Passing.s[,-1] , Offense_Rushing.s[,-1],Defense_Rushing.s[,-1],
                        Receiving.s[,-1], Returning.s[,-1], Kicking.s[,-1],Punting.s[,-1], Defense.s[,-1])
#Save data#
save(Team_Statistics, file = "Scraping/Team_Statistics.Rdata")
