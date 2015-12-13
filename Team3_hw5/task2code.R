load("task2.Rdata")


install.packages("d3heatmap")
library(d3heatmap)

# Use data2_2 to record full data frame
suppressWarnings(data2_2 <- data.frame(data2))
colnames(data2_2) <- c("hour", "wday", "gilded", "comments")
data_full <- tapply(data2_2[,4], data2_2[,1:2], sum)
colnames(data_full) <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# Interactive Heatmap
d3heatmap(data_full, scale="column", dendrogram = "none", color = "Blues")


# Use data2_3 to record data frame excluding rows where gilded = 0
data2_3 <- data2_2[which(data2_2$gilded != 0),]
data_gilded <- tapply(data2_3[,4], data2_3[,1:2], sum)
colnames(data_gilded) <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# Interactive Heatmap
d3heatmap(data_gilded, scale="column", dendrogram = "none", color = "Blues")

# Static Heatmap
png(filename="Full.png")
heatmap(data_full, scale="column", Rowv = NA, Colv = NA)
dev.off()

# Static Heatmap
png(filename="Gilded.png")
heatmap(data_gilded, scale="column", Rowv = NA, Colv = NA)
dev.off()