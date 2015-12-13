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

# sum reducer
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

# mapper for subreddit
# key is the subreddit, value is always 1
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

runHadoop <- function(file, mapper, reducer){
###########################################################
#  This function runs a map reduce job and returns a data frame
#
# Args
#   file:  json file that is target of code
#   mapper:  function that will serve as mapper in map reduce
#   reducer:  function that wil serve as reducer in map reduce
#
# Output
#   Returns a data frame.  First column is key, second is value
#   Note that key in mapper should be single value
#   I believe the code will return odd results if key is a list
###########################################################
  time = rhwatch(
    map      = mapper,
    reduce   = reducer,
    input    = rhfmt(file, type = "text")
  )
  # this converts the result to a data frame
  get_val = function(x,i) x[[i]]
  counts = data.frame(key = sapply(time,get_val,i=1),
                      value = sapply(time,get_val,i=2), 
                      stringsAsFactors=FALSE)
  return(counts)
}

list.of.files = list("/data/RC_2015-01.json", "/data/RC_2015-02.json",
                     "/data/RC_2015-03.json", "/data/RC_2015-04.json",
                     "/data/RC_2015-05.json")
current.files <- list.files(path = ".")
# run map reduce jobs only if .Rdata file is missing
if (!("reddit.jan.Rdata" %in% current.files)){
  jan.df <- runHadoop(list.of.files[[1]], sub_mapper, sum_reducer)
  save(jan.df, file = "reddit.jan.Rdata") # save result
}
if (!("reddit.feb.Rdata" %in% current.files)){
  feb.df <- runHadoop(list.of.files[[2]], sub_mapper, sum_reducer)
  save(feb.df, file = "reddit.feb.Rdata")
}
if (!("reddit.mar.Rdata" %in% current.files)){
  mar.df <- runHadoop(list.of.files[[3]], sub_mapper, sum_reducer)
  save(mar.df, file = "reddit.mar.Rdata")
}
if (!("reddit.apr.Rdata" %in% current.files)){
  apr.df <- runHadoop(list.of.files[[4]], sub_mapper, sum_reducer)
  save(apr.df, file = "reddit.apr.Rdata")
}
if (!("reddit.may.Rdata" %in% current.files)){
  may.df <- runHadoop(list.of.files[[5]], sub_mapper, sum_reducer)
  save(may.df, file = "reddit.may.Rdata")
}





