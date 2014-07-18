library(plyr)
n <- read.csv("names.csv")
m <- read.csv("names2.csv",
         header=FALSE, sep="$", skip=1, strip.white=TRUE, comment.char="#",
         col.names=c("first","last","birth.year","birth.country","comment"))
x <- read.csv("names3.csv", colClasses=c("character", "factor", "numeric"))