library(ggplot2)
i <- read.table("ictf2003.tsv",
                sep="\t",
                header=TRUE,  
                #nrows=30000, #just read the first 1M rows for now
                na.strings="",
                fill=TRUE,
                comment="", # appears in strings
                colClasses=c("character", rep("factor", 6), "character", rep("factor", 6)))
# Parse the time
i$frame.time <- as.POSIXct(strptime(i$frame.time, "%b  %e, %Y %H:%M:%OS"))
# Fix inconsistent case for some Connection headers
i$http.connection <- tolower(i$http.connection)
# Only look at the subset of packets which have a method (requests) or status codes (responses)
i.http <- i[((!is.na(i$http.request.method)) | !is.na(i$http.response.code)),]

# This graph shows the different http Connection headers, with traffic frequency per each source IP.
#g <- ggplot(i.http, aes(x=frame.time, y=http.request.method ,color=http.user_agent)) + geom_jitter()# + facet_grid(. ~ http.connection)
#ggsave(g,file="test.png", width=10)

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

# Lets do the same thing, but only show the top 5 source IPs
src.ip.counts <- as.data.frame(table(i.http.method$ip.src))
top5.ips <- as.character(head(src.ip.counts[order(-src.ip.counts$Freq),],n=5)$Var1)
# Filter the HTTP data
i.top5.http <- i.http.method[i.http.method$ip.src %in% top5.ips,]
# Plot a facet with each of the common HTTP methods, showing traffic for the top 5 source IPs
g <- ggplot(i.top5.http, aes(x=frame.time, y=ip.src ,color=http.request.method))+
  geom_jitter(size=0.7, alpha=0.7)+
  facet_wrap(~ http.request.method) +
  theme(legend.position="none") +
  xlab("Frame Timestamp") + ylab ("Source IP")
ggsave(g, file="top5.http_requests_by_method.all.png", width=8, height=4)

# qplot version of the above, without the additional subsetting, and without some additional labelling
qplot(frame.time, ip.src, data=i.top5.http, colour=http.request.method, facets=~http.request.method, geom="jitter")
