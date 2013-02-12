rm(list=ls(all=TRUE))

startime<-Sys.time()

library(twitteR)
library(stringr)
library(zoo)

inputfile <- "combinedretweet.csv"

df <- read.csv(file=inputfile)

retweetedge <- df[c(4,6,15)]
retweetedge = na.omit(retweetedge)
colnames(retweetedge)[1]<-"time"
colnames(retweetedge)[2]<-"source"
colnames(retweetedge)[3]<-"target"
retweetedge$target<-tolower(retweetedge$target)
retweetedge$source<-tolower(retweetedge$source)
#write.csv(retweetedge, file="retweetedgelist.csv", row.names=FALSE)
filename=paste("retweetedge", inputfile)
write.csv(retweetedge, file = filename, row.names=FALSE)
##################################################################################

viaedge <- df[c(4,6,16)]
viaedge = na.omit(viaedge)
colnames(viaedge)[1]<-"time"
colnames(viaedge)[2]<-"source"
colnames(viaedge)[3]<-"target"
viaedge$target<-tolower(viaedge$target)
viaedge$source<-tolower(viaedge$source)
#write.csv(retweetedge, file="retweetedgelist.csv", row.names=FALSE)
filename=paste("viaedge", inputfile)
write.csv(viaedge, file = filename, row.names=FALSE)
##################################################################################

modifiedtweetedge <- df[c(4,6,17)]
modifiedtweetedge = na.omit(modifiedtweetedge)
colnames(modifiedtweetedge)[1]<-"time"
colnames(modifiedtweetedge)[2]<-"source"
colnames(modifiedtweetedge)[3]<-"target"
modifiedtweetedge$target<-tolower(modifiedtweetedge$target)
modifiedtweetedge$source<-tolower(modifiedtweetedge$source)
#write.csv(retweetedge, file="retweetedgelist.csv", row.names=FALSE)
filename=paste("modifiedtweetedge", inputfile)
write.csv(modifiedtweetedge, file = filename, row.names=FALSE)
##################################################################################

tweetcounts=table(df$text)
uniquetweets<-length(tweetcounts)
tweetcounts=subset(tweetcounts, tweetcounts>1)
#write.csv(tweetcounts, file="tweetcounts.csv")
filename=paste("tweetcounts", inputfile)
write.csv(tweetcounts, file = filename, row.names=TRUE)

##################################################################################

df$link=str_extract_all(df$text, '(http://[[:graph:]\\s]*)')
linkunlist <- unlist(df$link)
linkcounts=table(linkunlist)
numberofuniquelinks<-length(linkcounts)
linkcounts<-as.data.frame(linkcounts)
colnames(linkcounts)[1]<-"link"
colnames(linkcounts)[2]<-"frequency"
filename=paste("linkcounts", inputfile)
write.csv(linkcounts, file = "linkcounts.csv", row.names=FALSE)
##################################################################################


endtime<-Sys.time()
runtime=endtime-startime
runtimemessage<-paste("This took", runtime)
print(runtimemessage)