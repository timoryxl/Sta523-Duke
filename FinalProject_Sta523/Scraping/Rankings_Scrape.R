library(rvest)
library(stringr)

#Read url#
url= "http://espn.go.com/college-football/rankings"
d = read_html(url)
abbr = html_nodes(d, ".team-names")

#Create empty matrix to hold rankings form cfp, ap, coaches and power#
Rankings = matrix(0, nrow = 25, ncol = 5)
Rankings[,1] = seq(1, 25, 1)

#The loop goes each ranking table within the url and store appropriate values#
count = 1
for(i in 2:5){
  for(j in 1:25){
    Rankings[j, i] = abbr[[count]]%>% html_text()
    count = count+1
  }
}

#Create dataframe, change names and save#
Rankings = as.data.frame(Rankings)
colnames(Rankings) = c("rank", "cfp", "ap", "coaches", "power")
save(Rankings, file = "Scraping/Rankings.Rdata")
