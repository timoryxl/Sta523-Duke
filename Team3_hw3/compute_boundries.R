library(data.table)
library(e1071)
library(dplyr)
library(nnet)
library(raster)
library(rgeos)
# this will be used to rename boroughs later
short_to_long = c("BK"="Brooklyn", 
                  "BX"="Bronx",
                  "MN"="Manhattan",
                  "QN"="Queens",
                  "SI"="Staten Island")
# load data file created by data_management.R
load("data/merged_data.Rdata")

# take needed columns
comp.data <- data.to.save[,c("y","x","Borough.y")]
# remove NAs
data.no.na <- comp.data[!apply(comp.data, 
                               1, 
                               function(x) {any(is.na(x))}),]
# change to factors
data.no.na$Borough.y <- as.factor(data.no.na$Borough.y)
# set sample size for training SVM
sam.size <- 50000
# take random sample of size sam.size to train SVM
train <- sample(nrow(data.no.na), sam.size)
# compute SVM prediction objection
svm.fit <- svm(data.no.na$Borough.y[train]~., data=data.no.na[train,1:2],
               kernel = "radial",
               gamma = 1,
               cost = .1)


## Create raster for prediction locations
# set size of raster object
size <- 175
# create raster object
r = raster(nrows=size, ncols=size, 
           xmn=-74.3, xmx=-73.71, 
           ymn=40.49, ymx=40.92)
# fill with NAs
r[] <- NA
# which cells in raster have data points in them
has.points <- cellFromXY(r, as.matrix(data.no.na[,c(2,1)]))
# remove duplicates from list
has.points <- unique(has.points)
# remove NAs from list
has.points  <- has.points[!is.na(has.points)]
# get centers from cells with points
pred_locs = data.frame(xyFromCell(r, has.points))
# name columns so can be used with predict
names(pred_locs) = c("x","y")
# predict borough for cells with points
r[has.points] = predict(svm.fit,pred_locs)

fillHole <- function(pos.vec){
#####################################################################
# This function fills "holes" in raster object.  If 5 of 8 neighbor
# cells belong to same borough, function replaces NA with borough
#
# Args:
#   pos.vec - a vector of length 2 location in raster matrix
#
# Output
#   There is no output.  Raster object is updated by function
#####################################################################
  # only run code if current cell is NA
  if (is.na(r[pos.vec[1],pos.vec[2]])){
    # get values of neighboring cells
    to.compare <- c(r[pos.vec[1]+1,pos.vec[2]],
    r[pos.vec[1]+1,pos.vec[2]+1],
    r[pos.vec[1]+1,pos.vec[2]-1],
    r[pos.vec[1]-1,pos.vec[2]],
    r[pos.vec[1]-1,pos.vec[2]+1],
    r[pos.vec[1]-1,pos.vec[2]-1],
    r[pos.vec[1],pos.vec[2]-1],
    r[pos.vec[1],pos.vec[2]+1])
    if (length(which(to.compare == 1)) >= 5){
      # if 5 of 8 in borough 1, replace NA with 1 in raster object
      r[pos.vec[1],pos.vec[2]] <<- 1
      # we are forced to use global assign to update raster object
    }
    if (length(which(to.compare == 2)) >= 5){
      r[pos.vec[1],pos.vec[2]] <<- 2
    }
    if (length(which(to.compare == 3)) >= 5){
      r[pos.vec[1],pos.vec[2]] <<- 3
    }
    if (length(which(to.compare == 4)) >= 5){
      r[pos.vec[1],pos.vec[2]] <<- 4
    }
    if (length(which(to.compare == 5)) >= 5){
      r[pos.vec[1],pos.vec[2]] <<- 5
    }
  }
}
# create matrix of all positions in raster object
# must avoid edges, otherwise will get out of bounds error
adj.dim <- 2:(size -1)
# make all pairs
input.matrix <- as.matrix(expand.grid(adj.dim,adj.dim))
# run function on pairs, do not print random stuff
invisible(apply(input.matrix, 1, fillHole))
  
# Create Polygons
bounds = rasterToPolygons(r,dissolve=TRUE)
# rename stuff to conform for wercker
names(bounds@data) = "Name"
bounds@data$Name = short_to_long[levels(data.no.na$Borough.y)]
# convert to json file
source("write_geojson.R")
write_geojson(bounds,"boroughs.json")

