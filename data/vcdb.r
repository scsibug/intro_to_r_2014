# Read in the Veris Community Database
vcdb <- read.csv("vcdb.csv")

# Start by investigating the data
# What are the columns?
str(vcdb)
# Decide to only investigate certain columns
vcdb <- vcdb[,c("incident_id", "summary", "discovery_method", "targeted", "actor.internal", "victim.country", "victim.industry")]

# Tabulate how many times each country appears as a victim
vc <- as.data.frame(table(vcdb$victim.country))
# Rename a column  (could have been done with dnn=c("country") arg to table())
colnames(vc)[1] <- "country"
top.vc <- head(vc[order(vc$Freq,decreasing=TRUE),],n=10)
plot(top.vc$Freq ~ droplevels(top.vc$country))
