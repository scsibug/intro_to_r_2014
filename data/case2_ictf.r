library(ggplot2)
library(plyr)
library(lubridate)
i <- read.table("ictf2003.tsv",
                sep="\t",
                header=TRUE,  
                nrows=100000, #just read the first 100k rows for now
                na.strings="",
                fill=TRUE,
                comment="", # appears in strings
                colClasses=c("character", rep("factor", 6), "character", rep("factor", 6)))
# Parse the time
i$frame.time <- as.POSIXct(strptime(i$frame.time, "%b  %e, %Y %H:%M:%OS"))

########### Plot traffic by protocol through duration of contest ###########
# 7M packets is a lot... lets summarize it by packets/minute for each protocol (TCP, UDP, ICMP)
i$frame.minute <- round_date(i$frame.time, "minute")
i.ppm <- ddply(i, .(frame.minute, ip.proto), summarise, packets=length(ip.proto))
# First, a simple line chart, overlaying all the protocols by color
g <- qplot(data=i.ppm, y=packets, x=frame.minute, geom="line", colour=ip.proto)
ggsave(g, file="packets_per_minute_by_proto.png", width=8, height=4)
# Or, a stacked barchart.  Again, color indicates the protocol
g <- qplot(data=i.ppm, x=frame.minute, y=packets, geom="bar", stat="identity", fill=ip.proto)
ggsave(g, file="stacked_packets_per_minute_by_proto.png", width=8, height=4)

########### Plot HTTP traffic by Method and IP ###########  
# Fix inconsistent case for some Connection headers
i$http.connection <- tolower(i$http.connection)
# Only look at the subset of packets which have a method (requests) or status codes (responses)
i.http <- i[((!is.na(i$http.request.method)) | !is.na(i$http.response.code)),]

# Select frequently used HTTP methods
i.http.method <- i.http[which(i.http$http.request.method %in% c("GET", "POST", "UPDATE", "HEAD")),]
# refactor some columns to remove all traces of methods and IPs which aren't in the new dataset
i.http.method$http.request.method <- factor(i.http.method$http.request.method)
i.http.method$ip.dst <- factor(i.http.method$ip.dst)
i.http.method$ip.src <- factor(i.http.method$ip.src)

# plot traffic over time by IP, color-coded HTTP methods, faceted by the method
g <- ggplot(i.http.method, aes(x=frame.time, y=ip.src ,color=http.request.method))+
  geom_jitter(size=2)+
  facet_wrap(~ http.request.method)
ggsave(g, file="http_requests_by_method.all.png", width=24, height=20)

########### Plot Top Source IP's HTTP traffic by Method and IP ###########
# With lots of hosts, this has too many labels.
# Lets do the same thing, but only show the top 5 source IPs

# Tabulate source IP's by number of packets
src.ip.counts <- as.data.frame(table(i.http.method$ip.src))
# Order the tabulation by frequency, take the top 5, and just get the IP
top5.ips <- head(src.ip.counts[order(-src.ip.counts$Freq),1],n=5)
# Filter the HTTP data to only those requests where the source is in the top 5
i.top5 <- i.http.method[i.http.method$ip.src %in% top5.ips,]
# Plot a facet with each of the common HTTP methods, showing traffic for the top 5 source IPs
# This demonstrates some of the additional control you get from using the full ggplot, instead of qplot
g <- ggplot(i.top5, aes(x=frame.time, y=ip.src ,color=http.request.method)) +
  geom_jitter(size=0.7, alpha=0.7) +
  facet_wrap(~http.request.method) +  # syntax is rows~columns
  theme(legend.position="none") + # disable legend
  xlab("Frame Timestamp") + ylab ("Source IP") # axis labels
ggsave(g, file="top5.http_requests_by_method.all.png", width=8, height=4)

# qplot version of the above, without the additional subsetting, and without some additional labelling
qplot(frame.time, ip.src, data=i.top5, colour=http.request.method, facets=~http.request.method, geom="jitter")

# Heatmap of the top 5 source and destination IPs
#find top 5 dest ips
dst.ip.counts <- as.data.frame(table(i.http.method$ip.dst))
# Order the tabulation by frequency, take the top 5, and just get the IP
top5.dst.ips <- head(dst.ip.counts[order(-dst.ip.counts$Freq),1],n=5)
# Filter the HTTP data to only those requests where the source is in the top 5
i.top5.dst <- i.http.method[i.http.method$ip.dst %in% top5.dst.ips,]
