#########################
### TEAM 3 - HW5 -Task2
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

# REDUCER : SUM REDUCER

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


# MAPPER : HOUR AND WEEKDAY (BASED ON LUBRIDATE) AND GILDED

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
      key = list(hour = hour, wday = wday, gilded = gilded)
      value = 1
      rhcollect(key,value)
    }
  )
})




# Now we only need to extract data for a month, so here Feburary data will be used
time_02 <- rhwatch(
  map      = hour_wday_gilded_map,
  reduce   = sum_reducer,
  input    = rhfmt("data/RC_2015-02.json", type = "text")
)

# We transform the returned list into a data frame for later use
# 610 rows and 4 columns
data2 <- as.numeric(unlist(time_02[[1]]))
for(i in 2:length(time_02)){
  data2 <- rbind(data2, as.numeric(unlist(time_02[[i]])))
}

save(data2, file = "task2.Rdata")