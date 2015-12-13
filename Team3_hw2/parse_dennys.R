require(magrittr) # need for pipe command
require(rvest) # need for read_html and other tags

# read in list of centers and radii to cover US
search.centers <- read.csv("dennys_coords.csv", header = FALSE)
num.of.files <- nrow(search.centers) # figure out how many files there are

# this is list of tags from which data will be collected
things.to.get <- c("latitude", "longitude", "phone","postalcode","state")

# this sapply recreates the list of file names that need to be analyzed
file.names <- sapply(1:num.of.files, function(x){paste0("data/dennys/",x,".xml")})
# read in each file and store the results in a list
read.results <- lapply(file.names, read_html)

# determine how many resturaunts are in each file, need to make df later
getCounts <- function(read.result){
#####################################################################
# This function determines how many results there were in a given API
# request
#
# Args
#   read.result:  a read-in XML file that can be parsed
#
# Output
#   An integer indicating the number of results in read.result
#####################################################################
  as.numeric(read.result %>% # start with file
               html_nodes("collection") %>% # go to node "collection"
               html_attr("count")) # get attribute of "count"
}

num.of.results <- sapply(read.results, getCounts) # get vec of counts


createDF <- function(list.index){
#####################################################################
# This function creates a data frame for a read-in XML file that is 
# located at a specific index of a list.  This function is intended
# for use with an apply function on the list read.results.  It will
# have columns corresponding to the elemnts of things.to.get vector
#
# Args
#   list.indix:  the position in the read.results list 
#
# Output
#   A data frame with specified data
#####################################################################
  do.call(cbind, # bind create columns into a data frame 
          lapply(things.to.get, function(x){
    # this function creates a column of the data frame
    my.result <- read.results[[list.index]] %>% # pick item from list
                html_nodes(x) %>% # x = "latitude", "longitude", etc
                html_text() # get text, what is actually in df
    return(my.result)
    }
  ))
}
dfs <- lapply(1:length(read.results), createDF) # make all data frames
one.df <- as.data.frame(do.call(rbind, dfs), 
                        stringsAsFactors = FALSE
                        ) # bind data frames together
final.df <- one.df[!duplicated(one.df),] # remove duplicates
colnames(final.df) <- things.to.get # name columns
row.names(final.df) <- 1:nrow(final.df)
final.df$latitude <- as.numeric(final.df$latitude) # convert to numeric
final.df$longitude <- as.numeric(final.df$longitude) # convert to numeric
save(final.df, file="data/dennys.Rdata") # save result as Rdata
