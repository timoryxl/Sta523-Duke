#########################
### TEAM 3 - HW5
#########################



#########################
### Hadoop initialization
#########################

Sys.setenv(HADOOP="/data/hadoop")
Sys.setenv(HADOOP_HOME="/data/hadoop")
Sys.setenv(HADOOP_BIN="/data/hadoop/bin") 
Sys.setenv(HADOOP_CMD="/data/hadoop/bin/hadoop") 
Sys.setenv(HADOOP_CONF_DIR="/data/hadoop/etc/hadoop") 
Sys.setenv(HADOOP_LIBS=system("/data/hadoop/bin/hadoop classpath | tr -d '*'",TRUE))

if (!("Rhipe" %in% installed.packages()))
{
  install.packages("/data/hadoop/rhipe/Rhipe_0.75.1.6_hadoop-2.tar.gz", repos=NULL)
}

library(Rhipe)
library(rJava)
rhinit()

## Uncomment following lines if you need non-base packages
rhoptions(zips = '/R/R.Pkg.tar.gz')
rhoptions(runner = 'sh ./R.Pkg/library/Rhipe/bin/RhipeMapReduce.sh')



#########################
### Short file names
#########################

# short_1e3.json
# short_1e6.json



#########################
### Long file names
#########################

# /data/ directory
# RC_2015-01.json
# RC_2015-02.json
# RC_2015-03.json
# RC_2015-04.json
# RC_2015-05.json



#########################
### Folder overview
#########################

rhls()
rhls("/data") # This folder contains all the files we are dealing with currently



#########################
### User Comment Example
#########################

# REDUCER 1: SUM REDUCER

sum_reducer = expression(
  pre = {
    total = 0
  },
  reduce = {
    total = total + sum(unlist(reduce.values))
  },
  post = {
    rhcollect(reduce.key, total)
  }
)

# MAPPER 1: HOUR

time_map = expression({
  suppressMessages(library(jsonlite))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      key = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%H')
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 2: DAY (OF THE MONTH)

day_map = expression({
  suppressMessages(library(jsonlite))
  
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      key = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%d')
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 3: WEEKDAY (BASED ON LUBRIDATE)

wday_map = expression({
  suppressMessages(library(jsonlite))
  suppressMessages(library(lubridate))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      key = wday(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), label = FALSE)
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 3A: HOUR AND WEEKDAY (BASED ON LUBRIDATE)

hour_wday_map = expression({
  suppressMessages(library(jsonlite))
  suppressMessages(library(lubridate))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      hour = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%H')
      wday = wday(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), label = FALSE)
      key = list(hour,wday)
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 3B: HOUR AND WEEKDAY (BASED ON LUBRIDATE) AND GILDED

hour_wday_gilded_map = expression({
  suppressMessages(library(jsonlite))
  suppressMessages(library(lubridate))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      hour = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%H')
      wday = wday(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), label = FALSE)
      gilded = fromJSON(map.values[[r]])$gilded
      key = list(hour,wday,gilded)
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 4: SUBREDDIT

sub_mapper = expression({
  suppressMessages(library(jsonlite))
  
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      key = fromJSON(map.values[[r]])$subreddit
      value = 1
      rhcollect(key,value)
    }
  )
})

# MAPPER 5: 2015-02-07 & 2015-02-14 & 2015-02-21
# Extract values for third task only

word_count_map = expression({
  suppressMessages(library(jsonlite))
  suppressMessages(library(magrittr))
  suppressMessages(library(stringr))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      day = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%d')
      if (day == "07" | day == "14" | day == "21" )
      {
        line = tolower(fromJSON(map.values[[r]])$body)
        line = gsub("[-—]"," ",line)
        line = gsub("[^'`’[:alpha:][:space:]]","",line,perl=TRUE)
        line = gsub("(^\\s+|\\s+$)","",line)
        line = strsplit(line, "\\s+")[[1]]
        line = line[line != ""]
        line = line[nchar(line) >3]
        
        lapply(line, function(x) rhcollect(key=list(word=x,day=day),value=1))
      } 
    }
  )
})


#########
# This function will return a data frame
# takes file (e.g. short_1e3.json), a reducer (e.g. sum_reduce)
# and a mapper (e.g. sub_mapper)
#########

runHadoop <- function(file, mapper, reducer){
  time = rhwatch(
    map      = mapper,
    reduce   = reducer,
    input    = rhfmt(file, type = "text")
  )
  
  get_val = function(x,i) x[[i]]
  counts = data.frame(key = sapply(time,get_val,i=1),value = sapply(time,get_val,i=2), stringsAsFactors=FALSE)
  return(counts)
}

# List of files for application to all datasets

list.of.files = list("/data/RC_2015-01.json", "/data/RC_2015-02.json",
                     "/data/RC_2015-03.json", "/data/RC_2015-04.json",
                     "/data/RC_2015-05.json")

# Extract Weekday Example (Small Dataset)

list.of.weekdays = lapply("data/short_1e3.json", function(x) {runHadoop(x, mapper=wday_map, reducer=sum_reducer)})

# Extract Hour (Small Dataset)

list.of.hours = lapply("data/short_1e3.json", function(x) {runHadoop(x, mapper=time_map, reducer=sum_reducer)})

# Extract Hour and Weekday (Small Dataset)

list.of.hour.and.weekday = lapply("data/short_1e3.json", function(x) {runHadoop(x, mapper=hour_wday_map, reducer=sum_reducer)})

# Extract Hour, Weekday, and Gilded (Small Dataset)

list.of.hour.weekday.and.gilded = lapply("data/short_1e3.json", function(x) {runHadoop(x, mapper=hour_wday_gilded_map, reducer=sum_reducer)})

# General formula (for all Datasets)

list.of.df = lapply(list.of.files, function(x) {runHadoop(x, mapper, reducer)})

# this code will sort a specified column in a df
# and return a specified number of rows
sortDF = function(df, num, col){
  topsubs = df[order(df[,col], decreasing = TRUE),][1:num,]
  return(topsubs)
}
# to sort all results in a list
sorted.list = lapply(list.of.df, function(x) {sortDF(x, 50, 2)})

time = rhwatch(
  map      = time_map,
  reduce   = sum_reducer,
  input    = rhfmt("/data/short_1e3.json", type = "text")
)


get_val = function(x,i) x[[i]]

counts = data.frame(key = sapply(time,get_val,i=1),value = sapply(time,get_val,i=2), stringsAsFactors=FALSE)



##############
# Potentially useful chunks of code
##############

# as.POSIXct(, origin="1970-01-01")

# lubridate

# wday(date)
# wday(date, label=T)
# key=list(wday, wday())

wday(as.POSIXct(, origin = '1970-01-01'), label = FALSE, abbr = TRUE)

# month = format(as.POSIXct(1420738741, origin = '1970-01-01'), format = '%m')
# key = paste(subreddit, month, sep = ":")

##########
# task 3 #
##########

# The REDUCER is the same as task 1 & 2

# MAPPER: 2015-02-07 & 2015-02-14 & 2015-02-21
word_count_map = expression({
  suppressMessages(library(jsonlite))
  suppressMessages(library(magrittr))
  suppressMessages(library(stringr))
  lapply(
    seq_along(map.keys), 
    function(r) 
    {
      day = format(as.POSIXct(as.numeric(fromJSON(map.values[[r]])$created_utc), origin = '1970-01-01'), format = '%d')
      if (day == "07" | day == "14" | day == "21" )
      {
        line = tolower(fromJSON(map.values[[r]])$body)
        line = gsub("[-—]"," ",line)
        line = gsub("[^'`’[:alpha:][:space:]]","",line,perl=TRUE)
        line = gsub("(^\\s+|\\s+$)","",line)
        line = strsplit(line, "\\s+")[[1]]
        line = line[line != ""]
        line = line[nchar(line) >3]
        
        lapply(line, function(x) rhcollect(key=list(word=x,day=day),value=1))
      } 
    }
  )
})

# Extract word, return a list of key and values pairs
time = rhwatch(
  map      = word_count_map,
  reduce   = sum_reducer,
  input    = rhfmt("/data/RC_2015-02.json", type = "text")
)

# get the word, day, value information and make them a data frame
get_word = function(x) x[[c(1,1)]]
get_day = function(x) x[[c(1,2)]]
get_value = function(x) x[[c(2,1)]]

word = data.frame(sapply(time,get_word),stringsAsFactors=FALSE)
day = data.frame(sapply(time,get_day))
value = data.frame(sapply(time,get_value))

counts = data.frame(word, day, value, stringsAsFactors = F)
names(counts) = c("word","day","value")

counts_07 = counts[which(counts$day=="07"),]
counts_14 = counts[which(counts$day=="14"),]
counts_21 = counts[which(counts$day=="21"),]

# list the words of which we want to compare the frequencies
words.of.interest = c("valentine","kiss","flowers","candy","chocolate","roses",
                      "girlfriend","boyfriend","wife","husband","significant","happy","date",
                      "movie","heart","memorize","dinner","gift")

# this function will return the corresponding frequency of each word of interest
get_count = function(data,x)
{
  count = data[which(data$word==x),]$value
  return(count)
}

# get the word frequencies for the Saturday before the Valentines' Day
before.valentine = unlist(lapply(words.of.interest,function(x) get_count(data = counts_07, x=x)))

# get the word frequencies for Valentines' Day
valentine = unlist(lapply(words.of.interest,function(x) get_count(data = counts_14, x=x)))

# get the word frequencies for the Saturday after the Valentines' Day
after.valentine = unlist(lapply(words.of.interest,function(x) get_count(data = counts_21, x=x)))

# make it a data frame
table = data.frame(words.of.interest,before.valentine,valentine,after.valentine,stringsAsFactors = TRUE)

# make it barplot
barplot(t(as.matrix(table[,-1])),
        names.arg=table$words.of.interest,
        las=2,
        col = c("darkblue","red","gray"),
        beside = TRUE,
        legend = c("Feb 7th","Feb 14th","Feb 21st"))
