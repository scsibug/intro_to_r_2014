# This case study shows how we use various R functions to investigate and cleanup a dataframe.
# original data source: http://ictf.cs.ucsb.edu/data/treasurehunt2002/

# Install the plyr package if you do not already have it...
# install.packages("plyr")
library(plyr)

# Read in some unknown data, just the first 5 lines.
s <-read.csv("treasurehunt_alpha_httpd_access.log",nrows=5)

# examine the "str"ucture
str(s)
# 5 obs. of 1 variable... Looks to be space-separated, not comma-separated like read.csv expects!
# Retry reading with read.table which separates by whitespace, read in more data
s <-read.table("treasurehunt_alpha_httpd_access.log", nrows=100)
# Many things that we can clean up here, through a more complex call to read.table
s <-read.table("treasurehunt_alpha_httpd_access.log",
               fill=TRUE,                # set missing elements at the end to blank (line 102 is incomplete)
               na.strings = "-",         # "-" is used to indicate missing values, set these to NA
               stringsAsFactors = FALSE) # don't default to making factors, we'll do this manually later
# Looks much better:  10 variables.
# lets assign some names
colnames(s) <- c("client", "identd", "userid", "date.1", "date.2", "request", "status", "size", "referrer", "useragent")
# we don't expect to get any value out of the "identd" column, so lets delete it
stopifnot(is.na(s$identd))
s$identd <- NULL
# Lets see if we can get rid of userid too:
summary(s$userid)
# no logged in users for any of these logs, so delete this as well
stopifnot(is.na(s$userid))
s$userid <- NULL
# examine the first few entries again with head()
head(s)

# Next item to cleanup: the date is broken into separate columns.
# s[,c("x","y")] selects all data from columns x and y from dataframe s.
# apply applies a function to each item in a vector
# paste concatenates strings together
s$req.date <- apply(s[,c("date.1","date.2")], 1, paste, collapse=" ")
# Lets parse these as real dates:
s$req.date <- as.POSIXct(strptime(s$req.date, "[%d/%b/%Y:%T %z]"))
# drop the old date columns
s[,c("date.1","date.2")] <- list(NULL)
# Turn status into a factor (it isn't really a "number", it is a categorical variable)
s$status <- factor(s$status)

# When there was no response, lets set these to 0 instead of NA.  It makes math work a bit better, and NA really means "zero bytes sent" in this case.
s$size <- with(s, ifelse(is.na(size),yes=0,no=size))

## Part 2: Joining with other data ##
# Goal, include geographical data about clients.
# An additional dataset, "treasure_hunt_geoip.csv" is provided that maps IPs into hostnames and locations.
# explicitly set factor and numeric types for data, blank strings are NA.
geoip <- read.csv("treasure_hunt_geoip.csv", colClasses=c(rep("factor",5),rep("numeric",2)),na.strings="")
# We want to match by either IP or host, so expand the geoip dataframe to combine these columns
# We will accomplish this by creating two copies of the data frame with a new "match" column that contains the IPs or the hostnames, and then combining them together with rbind
geoip.ip <- geoip # make a copy of the dataframe
geoip.host <- geoip # make another copy
geoip.ip$match <- geoip$IP # set the match variable to IP addresses
geoip.host$match <- geoip$host # set the match variable to hostnames
geoip.match <- rbind(geoip.ip, geoip.host) # combine both dataframes together
#### Same thing in a one-liner with plyr library
#### geoip.match <- ddply(geoip, .(IP), function (x) {y<-x; z<-x; y$match<-y$IP; z$match<-z$host; rbind(y,z)})
# delete items we are finished with
rm(geoip.ip, geoip.host)
# merge the "s" and "geoip.match" dataframes using the client and match columns, including everything from "s".
s.loc <- merge(s,geoip.match, by.x="client", by.y="match", all.x=TRUE)

# Check how many requests come from each city
table(s.loc$city, useNA="ifany")


# Now our data is in decent shape, and we can get learn from it:

# Various summary statistics (request count, total data, average request size) for each country in the logs
ddply(s.loc, .(country.code), summarize,
      size.total=sum(size, na.rm=TRUE), 
      size.mean=mean(size, na.rm=TRUE), 
      request.count=length(size))

# Max response size for each status code
ddply(s.loc, .(status), summarize, size.max=max(size, na.rm=TRUE))

# find the top-5 hosts by number of requests made
host.requests <- ddply(s.loc, .(host), summarize, request.count=length(host))
hosts.requests <- host.requests[order(host.requests$request.count, decreasing=TRUE),]
head(hosts.requests, n=5)
