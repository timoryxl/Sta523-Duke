# this block loads in all of the data frames
files <- c("reddit.jan.Rdata", 
           "reddit.feb.Rdata", 
           "reddit.mar.Rdata",
           "reddit.apr.Rdata",
           "reddit.may.Rdata")
for (i in 1:length(files)){
  load(files[i])
}
list.of.data <- list(jan.df, feb.df, mar.df, apr.df, may.df)


sortDF = function(df, num, col){
###########################################################
# This function sorts a data frame and returns a specified number of rows
#
# Args
#   df:  the data frame to be sorted
#   num:  the number of sorted rows to return
#   col:  the column with the number of posts in each subreddit (numeric column)
#
# Output
#   A sorted subset of the original data frame with a specifed number of rows
  ###########################################################
  topsubs = df[order(df[,col], decreasing = TRUE),][1:num,]
  return(topsubs)
}

# sort the loaded data, retaining only the top 200 subreddits each month
trunc.data <- lapply(list.of.data, function(x) {sortDF(x, 200, 2)})

findRow <- function(df1, df2, row, col.name){
###########################################################
# This function calculates the rank (row number) in previous month
#
# Args
#   df1:  top subsreddits in previous month
#   df2:  top subreddits in current month
#   row:  row (subreddit) from current month to match (should be iterated over)
#   col.name:  column with subreddit names. Should be same in both df
#
# Output
#   Returns the rank (row number) in previous month.  If not found
#   returns Inf
###########################################################
  prev.row <- which(df1[,col.name] == df2[row, col.name])
  if (length(prev.row) >= 1){ # if found
    return(prev.row[1])
  } else {
    return(Inf) # if not found in previous month
  }
}
makeDif <- function(df1, df2, col.name, keep){
###########################################################
#  Computes the change in rank for each row in df2
#  This function requires findRow function
#
# Args
#   df1: previous month ranked data
#   df2: current month ranked data
#   col.name:  number of column with subreddit names
#   keep:  number of rows to keep (say top 25)
#
# Output
#   Returns df2 with a new column indicating change in rank
###########################################################
  old.pos <- sapply(1:keep, function(x) {findRow(df1, df2, x, col.name)})
  pos.chg <- old.pos - 1:keep # change in position
  df <- cbind(df2[1:keep,], pos.chg) # merge and subset
  return(df)
}

cleanDF <- function(df, col.rank){
###########################################################
# This function cleans the data frame and reformats change in rank
#
# Args
#   df:  a data frame with subreddit, num of posts, and change in rank columns
#   col.rank:  the column with the change in rank from previous month
# 
# Output
#   Returns df with a reformatted column and new row and column names
###########################################################
  new.col <- vector(length = nrow(df))
  new.col[df[, col.rank] ==0] <- "--" # no change
  new.col[df[, col.rank] > 0] <- paste0("+", df[df[,col.rank] > 0, col.rank])  
  new.col[df[, col.rank] < 0] <- as.character(df[df[,col.rank] < 0, col.rank])
  new.col[is.infinite(df[, col.rank])] <- "New subreddit"
  df[,col.rank] <- new.col # replace old column
  colnames(df) <- c("Subreddit", "Number of Posts", "Change from last month")
  row.names(df) <- 1:nrow(df)
  return(df)
}
# get change for Feb-May (cannot do for January)
data.w.dif <- lapply(2:length(trunc.data), function(x) 
  {
  makeDif(trunc.data[[x-1]], trunc.data[[x]], 1, 25)
  })
# add column of 0s to January to indicate not change, keep 25 rows
jan.short <- as.data.frame(cbind(trunc.data[[1]][1:25,], 0))
# add this df to list
data.w.dif$jan <- jan.short
# name items in list
names(data.w.dif) <- c("feb", "mar", "apr", "may", "jan")
# clean all of the data frames
clean.df <- lapply(data.w.dif, function(x) {cleanDF(x, 3)})
# save the results
save(clean.df, file = "task1.Rdata")
