load("time.gzip")

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

save(table, file = "task3.Rdata")

# make it barplot
barplot(t(as.matrix(table[,-1])),
        names.arg=table$words.of.interest,
        las=2,
        col = c("darkblue","red","gray"),
        beside = TRUE,
        legend = c("Feb 7th","Feb 14th","Feb 21st"))
