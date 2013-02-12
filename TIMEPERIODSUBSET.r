#####THIS SCRIPT READS IN THE TIME PERIOD FILES AND THEN OUTPUTS MARKED UP VERSIONS THAT HAVE FAUXTO AND REPYTO DATA
#####THIS SCRIPT WAS RUN ON JANUARY 23 TO GET A DATASET OF ONLY CONVERSATIONS

###iconv -f iso-8859-1 -t utf-8 "officialdemocrat.csv" > "officialdemocrat.utf8.csv"
####erase all data in R
rm(list=ls(all=TRUE))

startime<-Sys.time()

library(twitteR)
library(stringr)
library(zoo)

###GET A FILE LIST FOR THE WORKING DIRECTORY
filelist<-list.files(getwd())

for (i in 1:length(filelist))
{

####input file based on the filelist 
inputfile <- filelist[i]

df <- read.csv(file=inputfile)
#####convert UTF-8 just to be safe, if the script is hanging up, it might be because of this
#df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))

##########need to create a set to delete columns

########################
df$from_user_id_str <- NULL
df$from_user_name <- NULL
df$to_user <- NULL
df$to_user_id_str <- NULL
df$to_user_name <- NULL
df$analysis_state <- NULL
df$location_geo <- NULL

numberoftweets<-nrow(df)
####function to trim the @ from the columns we extract
trimat <- function (x) sub('@','',x)
###function trims out period before the .@ messages
trimperiod <- function (x) sub('\\.','',x)
####take out colon
trimcolon <- function (x) sub(':','',x)
####take out comma
trimcomma <- function (x) sub(',','',x)
trimslash <- function (x) sub('\\/', '', x)

#####ADD TRIM QUOTE FUNCTION


#pull out reply to messages and create a new column
df$to=trimperiod(trimcomma(trimcolon(trimat(str_extract(df$text,"^(@[[:graph:]_]*)")))))

#pull out messages that preceded by a period, this functions as a public reply to
df$fauxto=trimcomma(trimcolon(trimperiod(trimat(str_extract(df$text,"^(.@[[:graph:]_]*)")))))

df$fauxtopresence=str_detect(df$text, "^(.@[[:graph:]_]*)")
fauxtopresence=table(df$fauxtopresence)
fauxtotruecount <- sum(df$fauxtopresence == 'TRUE')
fauxtofalsecount <- sum(df$fauxtopresence == 'FALSE')
fauxtopresencepercentage <- (fauxtotruecount/(fauxtotruecount+fauxtofalsecount))

df$replytopresence = str_detect(df$text, "^(@[[:graph:]_]*)")
replytopresence=table(df$replytopresence)
replytotruecount <- sum(df$replytopresence == 'TRUE')
replytofalsecount <- sum(df$replytopresence == 'FALSE')
replytopresencepercentage <- (replytotruecount/(replytotruecount+replytofalsecount))

#pull out retweets and create a new column
#####create trim RT to sub RT with a space and then pull out the retweeted individual
trimRT <- function (x) sub('RT ','',x)
df$retweet=trimslash(trimperiod(trimcomma(trimcolon(trimRT(trimat(str_extract(df$text,"RT (@[[:graph:]_]*)")))))))

####via is another form of retweet that we should examine
trimVIA <- function (x) sub('via ','',x)
df$via=trimperiod(trimcomma(trimcolon(trimVIA(trimat(str_extract(df$text, "via (@[[:graph:]_]*)"))))))

######MT indicates a modified tweet that is retweeted
trimMT <- function (x) sub('MT ','',x)
df$modifiedtweet=trimperiod(trimcomma(trimcolon(trimMT(trimat(str_extract(df$text,"MT (@[[:graph:]_]*)"))))))

##################################################################################
##################################################################################
######EXTRACTS DEVICE NAME BY EXTRACTING THE TEXT BETWEET &GT AND &LT AND PRINTS TO NEW COLUMN
####NOTE: SOMETIMES THE ACCESS STRING SHOWS UP AS "web" WITH NO WRAPPER SO THE PRINTING OF "character(0)" IN THE SOURCE FIELD INDICATES WEB
#####TRIM FUNCTIONS AT THE END ADDRESS SOME IRREGULARITIES IN THE CHARACTER STRINGS
trimLT <- function (x) sub('<','',x)
trimGT <- function (x) sub('>','',x)
trimBlackberry <- function (x) sub('\U3e65613c','',x)
trimUber <- function (x) sub('?ber','Uber',x)
####NOTE: SOMETIMES THE ACCESS STRING SHOWS UP AS "web" WITH NO WRAPPER SO THE PRINTING OF "character(0)" IN THE SOURCE FIELD INDICATES WEB ###THE BELOW ADDRESSES THAT, BUT CANNOT ELIMINATE (0) FOR NOW THIS IS FINE BECAUSE THAT (0) MAY BE INDICATIVE OF DIFFERENT ACCESS MECHANISM
trimWEB <- function (x) sub("character(0)",'web',x)
####without this trim function at the beginning it gives a multibyte error
df$source=trimLT(df$source)
df$source=trimGT(str_extract_all(df$source,'(>).*?(<)'))
df$source=trimLT(df$source)
df$source=trimBlackberry(df$source)
df$source=trimUber(df$source)
df$source=trimWEB(df$source)

##################################################################################
######ANALYSIS OF DEVICE COUNTS TAKEN FROM THE ABOVE
devicecounts=table(df$source)
numberofdevices <- length(devicecounts)
devicecounts<-as.data.frame(devicecounts)
colnames(devicecounts)[1]<-"device"
colnames(devicecounts)[2]<-"frequency"
#devicecounts=subset(devicecounts, devicecounts>1)
filename=paste("devicecounts", inputfile)
#write.csv(devicecounts, file = filename, row.names=FALSE)
##################################################################################
##################################################################################
###PRINTS EDGELIST FOR DEVICE AFFILIATION NETWORKS
######NEED TO FIGURE OUT HOW TO GET RID OF NA's in the data
###13 is the column in the data we output
deviceedge <- df[c(5,6)]
#deviceedge = na.omit(deviceedge)
#write.csv(deviceedge, file="deviceedgelist.csv", row.names=FALSE)
colnames(deviceedge)[1]<-"source"
colnames(deviceedge)[2]<-"target"
filename=paste("deviceedgelist", inputfile)
#write.csv(deviceedge, file = filename, row.names=FALSE)

##################################################################################
####LANGUAGE ANALYSIS
####TODO:::TRANSLATE THE LANGUAGE DIGRAPHS TO ACTUAL NAMES
###create edge list for languages right now
languageedge <- df[c(5,9)]

colnames(languageedge)[1]<-"source"
colnames(languageedge)[2]<-"target"
filename=paste("languageedgelist", inputfile)
#write.csv(languageedge, file = filename, row.names=FALSE)

###create table of languages used
language<-as.data.frame(table(df$iso_language))
###record column names
colnames(language)[1]<-"Language"
colnames(language)[2]<-"Frequency"
####determine number and percentage of english tweets
englishtweets<-length(subset(df$iso_language, df$iso_language=="en"))
percentofenglishtweets=englishtweets/numberoftweets
#write.csv(language, file="languagedistribution.csv", row.names=FALSE)

####identify individuals that used more than one languahe
uniquelanguageedge <- unique(languageedge)
uniquelanguageedgecounts=table(uniquelanguageedge$source)
uniquelanguageedgecountsusers=subset(uniquelanguageedgecounts, uniquelanguageedgecounts >1)
multiplelanguageusers <- length(uniquelanguageedgecountsusers)
#write.csv(uniquedeviceedgecountsusers, file="uniquedeviceedgecountsusers.csv")
###output file of users and number of languages they used
filename=paste("uniquelanguageedgecounts", inputfile)
#write.csv(uniquelanguageedgecounts, file = filename, row.names=TRUE)

###print out a table of individuals who used more than one language
filename=paste("multiplelanguageusers", inputfile)
#write.csv(uniquelanguageedgecountsusers, file=filename, row.names=TRUE)

####GEOGRAPHIC ANALYSIS
missinggeo<-sum(is.na(df$location_geo_1))
geopresence=1-(missinggeo/numberoftweets)

#####TEXT ANALYSIS
####returns the length of the df$text column which is the average number of characters
characterlength<-nchar(as.character(df$text))
dfcharacterlength<-as.data.frame(characterlength)
meantweetcharacterlength<-colMeans(dfcharacterlength)

##################################################################################
####identifies individuals who have used more than one device
uniquedeviceedge <- unique(deviceedge)
uniquedevicedgecounts=table(uniquedeviceedge$source)
uniquedeviceedgecountsusers=subset(uniquedevicedgecounts, uniquedevicedgecounts >1)
multipledeviceusers <- length(uniquedeviceedgecountsusers)
#write.csv(uniquedeviceedgecountsusers, file="uniquedeviceedgecountsusers.csv")
filename=paste("uniquedeviceedgecountsusers", inputfile)
#write.csv(uniquedeviceedgecountsusers, file = filename, row.names=TRUE)
##################################################################################
##################################################################################
####calculate percentage of users who use more than 1 device/application
t<-length(uniquedeviceedgecountsusers)
k<-length(uniquedevicedgecounts)
deviceduplicatepercentage=(t/k)
##################################################################################
##################################################################################
##################################################################################
##################################################################################
###DETECTS PRESENCE OF A LINK/HASHTAG/MENTION IN A BODY OF TEXT AND PRINTS TRUE/FALSE
df$linkpresence=str_detect(df$text, '(http................)')
linkpresence=table(df$linkpresence)
linktruecount <- sum(df$linkpresence == 'TRUE')
linkfalsecount <- sum(df$linkpresence == 'FALSE')
linkpresencepercentage <- (linktruecount/(linktruecount+linkfalsecount))
#write.csv(linkpresence, file="linkpresence.csv")

df$fauxtopresence=str_detect(df$text, "^(.@[[:graph:]_]*)")
fauxtopresence=table(df$fauxtopresence)
fauxtotruecount <- sum(df$fauxtopresence == 'TRUE')
fauxtofalsecount <- sum(df$fauxtopresence == 'FALSE')
fauxtopresencepercentage <- (fauxtotruecount/(fauxtotruecount+fauxtofalsecount))

df$replytopresence = str_detect(df$text, "^(@[[:graph:]_]*)")
replytopresence=table(df$replytopresence)
replytotruecount <- sum(df$replytopresence == 'TRUE')
replytofalsecount <- sum(df$replytopresence == 'FALSE')
replytopresencepercentage <- (replytotruecount/(replytotruecount+replytofalsecount))


df$hashpresence=str_detect(df$text, "(#[[:graph:]_]*)")
hashpresence=table(df$hashpresence)
hashtruecount <- sum(df$hashpresence == 'TRUE')
hashfalsecount <- sum(df$hashpresence == 'FALSE')
hashpresencepercentage <- (hashtruecount/(hashtruecount+hashfalsecount))
#write.csv(hashpresence, file="hashpresence.csv")

df$mentionpresence = str_detect(df$text, "(@[[:graph:]_]*)")
mentionpresence=table(df$mentionpresence)
mentiontruecount <- sum(df$mentionpresence == 'TRUE')
mentionfalsecount <- sum(df$mentionpresence == 'FALSE')
mentionpresencepercentage <- (mentiontruecount/(mentiontruecount+mentionfalsecount))
#write.csv(mentionpresence, file="mentionpresence.csv")

df$rtpresence = str_detect(df$text, "RT (@[[:graph:]_]*)")
rtpresence=table(df$rtpresence)
rttruecount <- sum(df$rtpresence == 'TRUE')
rtfalsecount <- sum(df$rtpresence == 'FALSE')
rtpresencepercentage <- (rttruecount/(rttruecount+rtfalsecount))
#write.csv(rtpresence, file="rtpresence.csv")

df$viapresence=trimVIA(trimat(str_detect(df$text, "via (@[[:graph:]_]*)")))
viapresence=table(df$viapresence)
viatruecount <- sum(df$viapresence == 'TRUE')
viafalsecount <- sum(df$viapresence == 'FALSE')
viapresencepercentage <- (viatruecount/(viatruecount+viafalsecount))
#write.csv(viapresence, file="viapresence.csv")

df$mtpresence=trimVIA(trimat(str_detect(df$text, "MT (@[[:graph:]_]*)")))
mtpresence=table(df$mtpresence)
mttruecount <- sum(df$mtpresence == 'TRUE')
mtfalsecount <- sum(df$mtpresence == 'FALSE')
mtpresencepercentage <- (mttruecount/(mttruecount+mtfalsecount))
#write.csv(mtpresence, file="mtpresence.csv")

####DETECTS THE NUMBER OF LINKS/HASHTAG/MENTION IN EACH OF THE COLUMNS AND PRINTS THE NUMBER
df$linknumber=str_count(df$text, '(http................)')
linknumber=table(df$linknumber)
#write.csv(linknumber, file="linknumber.csv")
filename=paste("linknumber", inputfile)
#write.csv(linknumber, file = filename, row.names=TRUE)

df$hashnumber=str_count(df$text, "(#[[:graph:]_]*)")
hashnumber=table(df$hashnumber)
#write.csv(hashnumber, file="hashnumber.csv")
filename=paste("hashnumber", inputfile)
#write.csv(hashnumber, file = filename, row.names=TRUE)

df$mentionnumber = str_count(df$text, "(@[[:graph:]_]*)")
mentionnumber=table(df$mentionnumber)
#write.csv(mentionnumber, file="mentionednumber.csv")
filename=paste("mentionnumber", inputfile)
#write.csv(mentionnumber, file = filename, row.names=TRUE)

##################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
##################################################################################


###############################
###############
####set working directory to write the marked up file to
setwd("/Users/cmascaro/Desktop/Dissertation Time Periods/MARKED UP TIME PERIODS")
################
###############################
###############################


filename=paste("MARKEDUP", inputfile)
write.csv(df, file = filename, row.names=FALSE)

######reset working directory to where the files are
setwd("/Users/cmascaro/Desktop/Dissertation Time Periods/Time Periods RAW")

endtime<-Sys.time()
runtime=endtime-startime
runtimemessage<-paste("This took", runtime)
print(runtimemessage)
}


################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############
################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############

################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############

################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############

################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############

################
###############################
###############################
###############################
###############################
###############################
###############################
###############################
###############

