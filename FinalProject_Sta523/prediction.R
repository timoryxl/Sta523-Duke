library(dplyr)
library(magrittr)
library(glmnet)
set.seed(87)
#Load in team data/statistics, school info and rankings#
load("Scraping/Wiki_team.Rdata")
load("Scraping/Rankings.Rdata")
load("Scraping/Team_Statistics.Rdata")
Offense = read.csv("Scraping/Offense.csv") %>%
  as.data.frame()
Defense = read.csv("Scraping/Defense.csv") %>%
  as.data.frame()




#Select variables of interest for prediction/clustering#
Offense = Offense %>%
  select(Team, total_PTS.G, efficiency_OFFENSE, efficiency_OVERALL, 
         total_YDS.G, passing_YDS.G, rushing_YDS.G, 
         passing_PCT, passing_YDS.A:passing_RAT,rushing_YDS.A:rushing_TD, 
         firstdowns_RUSH:firstdowns_PEN, thirddowns_PCT, fourthdowns_PCT,
         penalties_TOTAL, penalties_YDS) %>%
  mutate(YDS.Pen = penalties_YDS/penalties_TOTAL) %>%
  select(-(penalties_TOTAL:penalties_YDS))


Defense = Defense %>%
  select(Team, totaldefense__PTS.G, efficiency_DEFENSE, efficiency_OVERALL,
         totaldefense__YDS.G, totaldefense__P.YDS.G, totaldefense__R.YDS.G,
         passingYards_PCT, passingYards_YDS.A:passingYards_RAT, 
         rushingdefense__YDS.A:rushingdefense__TD, tackles_TOTAL,
         interceptions_YDS:fumbless_REC)


SpecialTeams = Team_Statistics %>%
  select(Offense_Total_TEAM, 
         Punting_AVG, Kicking_FieldGoals_PCT, Kicking_ExtraPoints_PCT, 
         Returning_Kickoffs_AVG, Returning_Punts_AVG) %>%
  mutate(Punting_AVG = as.numeric(Punting_AVG), 
         Kicking_FieldGoals_PCT = as.numeric(Kicking_FieldGoals_PCT), 
         Kicking_ExtraPoints_PCT = as.numeric(Kicking_ExtraPoints_PCT), 
         Returning_Kickoffs_AVG = as.numeric(Returning_Kickoffs_AVG), 
         Returning_Punts_AVG = as.numeric(Returning_Punts_AVG))

##Add special teams data to offense and defense dataframes##
Offense = data.frame(Offense, SpecialTeams[,-(1:2)])
Defense = data.frame(Defense, SpecialTeams$Punting_AVG)
Wiki_team = Wiki_team %>%
  rename(Current.Conference = `Current
Conference`) %>%
  rename(Longitude = Longtitude) %>%
  rename(Latitude = Latiutude)
Wiki_team$Current.Conference[which(Wiki_team$Current.Conference == "MAC[6]")] = "MAC"
##Separate rankings to format indicator for regression##
##Add indicator, 2 indicators for each poll#
##1 in rank 5 or higher for first indicator, 1 if rank between 6 and 25 for other##

rankCFP = data.frame(Team = as.character(Rankings$cfp), CFP = 1:25) 
  
rankAP = data.frame(Team = as.character(Rankings$ap), AP = 1:25)
rankCombined = full_join(rankCFP, rankAP, by = "Team") %>%
  mutate(CFP.5 = ifelse(0<CFP &  CFP <=5, 1, 0)) %>%
  mutate(CFP.6_25 = ifelse(CFP > 5, 1, 0)) %>%
  mutate(AP.5 = ifelse(0<AP &  AP <=5, 1, 0)) %>%
  mutate(AP.6_25 = ifelse(AP > 5, 1, 0)) 

##Add rankings for CFP and AP to Offense and Defense data frames##
##Change NAs for rankings to 0##
Defense = full_join(Defense, rankCombined, by = "Team") %>%
  select(-CFP, -AP)
Defense[is.na(Defense)] = 0  
rownames(Defense) = Defense[,1]  
Defense = Defense[,-1]
  
Offense = full_join(Offense, rankCombined, by = "Team") %>%
  select(-CFP, -AP)
Offense[is.na(Offense)] = 0  
rownames(Offense) = Offense[,1]  
Offense = Offense[,-1]

rownames(Wiki_team) = Wiki_team[,1]
Wiki_team = Wiki_team[,-1]
####################
##LASSO REGRESSION##
####################


##Perform LASSO Regression on Offense/Defense stats to predict points/game##
##Using training data to estimate predictive ability of Frequentist vs. Bayesian##
##Regression by comparing MSE values##

###########
##OFFENSE##
###########

samp = sample(1:100, size = 80)
trainO = Offense[samp,]
i = which(colnames(Offense) == "total_PTS.G" )
trainO.cv = cv.glmnet(x = as.matrix(trainO[,-i]), 
                    y = as.numeric(trainO$total_PTS.G), alpha = 1)
r.offense = glmnet(x = as.matrix(trainO[,-i]), 
                y = as.numeric(trainO$total_PTS.G), alpha = 1, 
                lambda = trainO.cv$lambda.min)
testO = data.frame(Intercept = 1, Offense[-samp, -i])
predict.O = as.matrix(testO) %*% as.matrix(coef(r.offense))
MSE.O = sum(Offense[-samp, i] - predict.O)^2/dim(Offense[-samp,])[1]
coef(r.offense)

###########
##DEFENSE##
###########

trainD = Defense[samp,]
j = which(colnames(Defense) == "totaldefense__PTS.G" )
trainD.cv = cv.glmnet(x = as.matrix(trainD[,-j]), 
                      y = as.numeric(trainD$totaldefense__PTS.G), alpha = 1)
r.defense = glmnet(x = as.matrix(trainD[,-j]), 
                   y = as.numeric(trainD$totaldefense__PTS.G), alpha = 1, 
                   lambda = trainD.cv$lambda.min)
testD = data.frame(Intercept = 1, Defense[-samp, -j])
predict.D = as.matrix(testD) %*% as.matrix(coef(r.defense))
MSE.D = sum(Defense[-samp, j] - predict.D)^2/dim(Defense[-samp,])[1]
coef(r.defense)


###########################
##KMEANS CLUSTERING##
###########################

##Number of clusters k is equal to true number of football conferences, which is 11##
k = length(unique((Wiki_team$Current.Conference)))
cluster.O = kmeans(dist(Offense), centers=k, nstart=10, 
                   iter.max = 100, algorithm="Lloyd")
cluster.D = kmeans(dist(Defense), centers=k, nstart=10, 
                   iter.max = 100, algorithm="Lloyd")
cluster.total = kmeans(dist(data.frame(Offense, Defense)), centers=k, nstart=10, 
                       iter.max = 100, algorithm="Lloyd")
location = Wiki_team %>%
  select(Longitude, Latitude)
cluster.geo = kmeans(dist(location), centers=k, nstart=10, 
                     iter.max = 100, algorithm="Lloyd")
