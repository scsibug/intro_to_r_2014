i <- read.table("ictf2003.tsv",
                sep="\t",
                header=TRUE,  
                #nrows=3000000, #just read the first 1M rows for now
                na.strings="",
                fill=TRUE,
                comment="", # appears in strings
                colClasses=c("character", rep("factor", 6), "character", rep("factor", 6)))
i$frame.time <- as.POSIXct(strptime(i$frame.time, "%b  %e, %Y %H:%M:%OS"))
i$http.connection <- tolower(i$http.connection)
i.http <- i[!(is.na(i$http.request.method)),]
# This graph shows the different http Connection headers, with traffic frequency per each source IP.
ggplot(i.http, aes(x=frame.time, y=ip.src ,color=http.user_agent)) +geom_point() + geom_jitter() + facet_grid(. ~ http.connection)

rm(g)
g <- ggplot(i.http, aes(x=frame.time, y=ip.src ,color=http.request.method))+ geom_point(size=2) + geom_jitter(size=2) #+ geom_jitter() #+ facet_grid(. ~ http.connection)
ggsave(g, file="http_requests_by_method.all.png", width=10, height=8)
