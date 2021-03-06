##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
################Christopher Mascaro, Dissertation Parsing Script################
####THIS SCRIPT INPUTS A SERIES OF PARSED FILES BASED ON DATE OR SOME OTHER FEATURE AND PERFORMS ANALYSIS####
####PARSING OF FILES IS A RESULT OF R'S LIMITATIONS ON MEMORY USAGE ON MOST MACHINES#####
#####NOTE: Some packages may have dependencies for R 2.15####
##############################################################################################
##############################################################################################
##############################################################################################
##################################################################################
#########THIS SECTION NORMALIZES THE DATA#######
###identify start time
startime<-Sys.time()

library(twitteR)
library(stringr)
library(zoo)

####set working directory
setwd("/Users/cmascaro/Desktop/Dissertation Time Periods/ScriptDev")

###GET A FILE LIST FOR THE WORKING DIRECTORY OF THE data, this allows for multiple files to be parsed in one run
###important for large analysis, set it up and walk away if the appropriate files are in the directory
filelist<-list.files(getwd())

for (i in 1:length(filelist))
{
####input file
inputfile <- filelist[i]

df <- read.csv(file=inputfile)
#####convert UTF-8 just to be safe, if the script is hanging up, it might be because of this
#df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))

##########need to create a set to delete columns

title<-filelist[i]

###create directory based on the title
dirname=paste(title, "")

dir.create(dirname)

####set working directory to new filename
setwd(dirname)

#######################
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

#pull out reply to messages and create a new column
df$to=trimperiod(trimcomma(trimcolon(trimat(str_extract(df$text,"^(@[[:graph:]_]*)")))))

#pull out messages that preceded by a period, this functions as a public reply to
df$fauxto=trimcomma(trimcolon(trimperiod(trimat(str_extract(df$text,"^(.@[[:graph:]_]*)")))))

#pull out retweets and create a new column
#####create trim RT to sub RT with a space and then pull out the retweeted individual
trimRT <- function (x) sub('rt ','',x)
df$retweet=trimslash(trimperiod(trimcomma(trimcolon(trimRT(trimat(str_extract(df$text,"rt (@[[:graph:]_]*)")))))))

####via is another form of retweet that we should examine
trimVIA <- function (x) sub('via ','',x)
df$via=trimperiod(trimcomma(trimcolon(trimVIA(trimat(str_extract(df$text, "via (@[[:graph:]_]*)"))))))

######MT indicates a modified tweet that is retweeted
trimMT <- function (x) sub('MT ','',x)
df$modifiedtweet=trimperiod(trimcomma(trimcolon(trimMT(trimat(str_extract(df$text,"MT (@[[:graph:]_]*)"))))))

###lowercase text for analysis, normalizes text to account for case differences
df$text<-tolower(df$text)

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
write.csv(devicecounts, file = filename, row.names=FALSE)
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
write.csv(deviceedge, file = filename, row.names=FALSE)

##################################################################################
####LANGUAGE ANALYSIS
###create edge list for languages right now
languageedge <- df[c(5,9)]

colnames(languageedge)[1]<-"source"
colnames(languageedge)[2]<-"target"
filename=paste("languageedgelist", inputfile)
write.csv(languageedge, file = filename, row.names=FALSE)

###create table of languages used
language<-as.data.frame(table(df$iso_language))
###record column names
colnames(language)[1]<-"Language"
colnames(language)[2]<-"Frequency"
####determine number and percentage of english tweets
englishtweets<-length(subset(df$iso_language, df$iso_language=="en"))
percentofenglishtweets=englishtweets/numberoftweets
write.csv(language, file="languagedistribution.csv", row.names=FALSE)

####identify individuals that used more than one languahe
uniquelanguageedge <- unique(languageedge)
uniquelanguageedgecounts=table(uniquelanguageedge$source)
uniquelanguageedgecountsusers=subset(uniquelanguageedgecounts, uniquelanguageedgecounts >1)
multiplelanguageusers <- length(uniquelanguageedgecountsusers)
#write.csv(uniquedeviceedgecountsusers, file="uniquedeviceedgecountsusers.csv")
###output file of users and number of languages they used
filename=paste("uniquelanguageedgecounts", inputfile)
write.csv(uniquelanguageedgecounts, file = filename, row.names=TRUE)

###print out a table of individuals who used more than one language
filename=paste("multiplelanguageusers", inputfile)
write.csv(uniquelanguageedgecountsusers, file=filename, row.names=TRUE)

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
write.csv(uniquedeviceedgecountsusers, file = filename, row.names=TRUE)
##################################################################################
##################################################################################
####calculate percentage of users who use more than 1 device/application
t<-length(uniquedeviceedgecountsusers)
k<-length(uniquedevicedgecounts)
deviceduplicatepercentage=(t/k)
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

df$replytopresence = str_detect(df$text, "^(@[[:graph:]_]*)")
replytopresence=table(df$replytopresence)
replytotruecount <- sum(df$replytopresence == 'TRUE')
replytofalsecount <- sum(df$replytopresence == 'FALSE')
replytopresencepercentage <- (replytotruecount/(replytotruecount+replytofalsecount))
#write.csv(replytopresence, file="replytopresence.csv")

df$rtpresence = str_detect(df$text, "rt (@[[:graph:]_]*)")
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

df$mtpresence=trimVIA(trimat(str_detect(df$text, "rt (@[[:graph:]_]*)")))
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
write.csv(linknumber, file = filename, row.names=TRUE)

df$hashnumber=str_count(df$text, "(#[[:graph:]_]*)")
hashnumber=table(df$hashnumber)
#write.csv(hashnumber, file="hashnumber.csv")
filename=paste("hashnumber", inputfile)
write.csv(hashnumber, file = filename, row.names=TRUE)

df$mentionnumber = str_count(df$text, "(@[[:graph:]_]*)")
mentionnumber=table(df$mentionnumber)
#write.csv(mentionnumber, file="mentionednumber.csv")
filename=paste("mentionnumber", inputfile)
write.csv(mentionnumber, file = filename, row.names=TRUE)

#####pull identify the presence of the candidates for syntactical feature overview

df$obamareplytopresence=(str_detect(df$text, "^@barackobama"))
df$obamaretweetpresence=(str_detect(df$text, "rt @barackobama"))
df$obamapresence=(str_detect(df$text, "@barackobama"))
obamapresence=table(df$obamapresence)
obamatruecount <- sum(df$obamapresence == 'TRUE')
obamafalsecount <- sum(df$obamapresence == 'FALSE')
obamapresencepercentage <- (obamatruecount/(obamatruecount+obamafalsecount))
df$obamafauxpresence=(str_detect(df$text, "^.@barackobama"))
df$obamalocate=(str_locate(df$text, "@barackobama"))

obamasubset<-subset(df, df$obamapresence=='TRUE')
filename=paste("obamasubset", inputfile)
write.csv(obamasubset, file=filename)

df$bidenreplytopresence=(str_detect(df$text, "^@joebiden"))
df$bidenretweetpresence=(str_detect(df$text, "rt @joebiden"))
df$bidenpresence=(str_detect(df$text, "@joebiden"))
bidenpresence=table(df$bidenpresence)
bidentruecount <- sum(df$bidenpresence == 'TRUE')
bidenfalsecount <- sum(df$bidenpresence == 'FALSE')
bidenpresencepercentage <- (bidentruecount/(bidentruecount+bidenfalsecount))
df$bidenfauxpresence=(str_detect(df$text, "^.@joebiden"))
df$bidenlocate=(str_locate(df$text, "@joebiden"))

bidensubset<-subset(df, df$bidenpresence=='TRUE')
filename=paste("bidensubset", inputfile)
write.csv(bidensubset, file=filename)

df$mittreplytopresence=(str_detect(df$text, "^@mittromney"))
df$mittretweetpresence=(str_detect(df$text, "rt @mittromney"))
df$mittpresence=(str_detect(df$text, "@mittromney"))
mittpresence=table(df$mittpresence)
mitttruecount <- sum(df$mittpresence == 'TRUE')
mittfalsecount <- sum(df$mittpresence == 'FALSE')
mittpresencepercentage <- (mitttruecount/(mitttruecount+mittfalsecount))
df$mittfauxpresence=(str_detect(df$text, "^.@mittromney"))
df$mittlocate=(str_locate(df$text, "@mittromney"))

mittsubset<-subset(df, df$mittpresence=='TRUE')
filename=paste("mittsubset", inputfile)
write.csv(mittsubset, file=filename)

df$ryanreplytopresence=(str_detect(df$text, "^@paulryanvp"))
df$ryanretweetpresence=(str_detect(df$text, "rt @paulryanvp"))
df$ryanpresence=(str_detect(df$text, "@paulryanvp"))
ryanpresence=table(df$ryanpresence)
ryantruecount <- sum(df$ryanpresence == 'TRUE')
ryanfalsecount <- sum(df$ryanpresence == 'FALSE')
ryanpresencepercentage <- (ryantruecount/(ryantruecount+ryanfalsecount))
df$ryanfauxpresence=(str_detect(df$text, "^.@paulryanvp"))
df$ryanlocate=(str_locate(df$text, "@paulryanvp"))

ryansubset<-subset(df, df$ryanpresence=='TRUE')
filename=paste("ryansubset", inputfile)
write.csv(ryansubset, file=filename)

######write annotated file back to disk
filename=paste("MARKEDUP", inputfile)
write.csv(df, file = filename, row.names=FALSE)

##################################################################################
##################################################################################
######IDENTIFY FREQUENT TWEETS, THIS IS LIKELY TO MOSTLY BE RETWEETS, BUT CAN BE ANY TWEET, IT IS ALSO LIKELY TO PULL OUT TWEETS THAT JUST CONTAIN ONE COMMON HASHTAG, ONE DOWNSIDE IS THAT IF ONE CHARACTER IS OFF THEN IT WILL NOT WORK CORRECTLY
tweetcounts=table(df$text)
uniquetweets<-length(tweetcounts)
tweetcounts=subset(tweetcounts, tweetcounts>1)
#write.csv(tweetcounts, file="tweetcounts.csv")
filename=paste("tweetcounts", inputfile)
write.csv(tweetcounts, file = filename, row.names=TRUE)
##################################################################################
##################################################################################
#######IDENTIFY THOSE WHO HAVE BEEN RETWEETED THE MOST 
retweetcounts<-as.data.frame(table(df$retweet))
colnames(retweetcounts)[1]<-"retweeted"
colnames(retweetcounts)[2]<-"frequency"
numberofpeopleretweeted <- nrow(retweetcounts)
#write.csv(retweetcounts, file="retweetcounts.csv")
filename=paste("retweetcounts", inputfile)
write.csv(retweetcounts, file = filename, row.names=FALSE)
##################################################################################
##################################################################################
#######IDENTIFY THOSE WHO HAVE TWEETED THE MOST  
tweetscreenamecounts=df$from_user
tweetscreenamecounts=tolower(tweetscreenamecounts)
tweetusercounts<-table(tweetscreenamecounts)
tweetscreenamecounts<-as.data.frame(table(tweetscreenamecounts))
colnames(tweetscreenamecounts)[1]<-"user"
colnames(tweetscreenamecounts)[2]<-"frequency"
totalusers=tolower(df$from_user)
totalusers=unique(totalusers)
###calculate number of single posters
singleton<-subset(tweetusercounts, tweetusercounts<2)
numberofsingleposters<-length(singleton)
numberofuniquetweeters <- length(totalusers)
####CALCULATE PERCENTAGE OF SINGLETON POSTERS
singletonpercentage<-numberofsingleposters/numberofuniquetweeters
filename=paste("tweetscreenamecounts", inputfile)
write.csv(tweetscreenamecounts, file = filename, row.names=FALSE)
#################
##################################################################################
##################################################################################
########EXTRACTS HASHTAGS, THEN TRIMS THE # OUT, MAKES THEM ALL LOWERCASE AND THEN OUTPUTS A FREQUENCY COUNT OF THE OCCURRENCES OF THE HASHTAGS
hash = str_extract_all(df$text, "(#[[:graph:]_]*)")
hashunlist <- unlist(hash)
trimhash <- function (x) sub('#','',x)
hashunlist <- trimhash(hashunlist)
lowerhash <- tolower(hashunlist)
lowerhash<-trimcomma(trimperiod(trimslash(trimcolon(lowerhash))))
hashcounts<-table(lowerhash)
hashcounts<-as.data.frame(hashcounts)
numberofuniquehashtags<-nrow(hashcounts)
colnames(hashcounts)[1]<-"hashtag"
colnames(hashcounts)[2]<-"frequency"
filename=paste("hashcounts", inputfile)
write.csv(hashcounts, file = filename, row.names=FALSE)
##################################################################################
##################################################################################
########EXTRACTS MENTIONS, THEN TRIMS THE @ OUT, AND THEN OUTPUTS A FREQUENCY COUNT OF THE OCCURRENCES OF THE HASHTAGS 
mention = str_extract_all(df$text, "(@[[:graph:]_]*)")
mentionunlist <- unlist(mention)
mentionunlist <- trimat(mentionunlist)
mentionunlist <- trimcolon(mentionunlist)
mentionunlist <- trimperiod(mentionunlist)
mentionunlist <- trimcomma(mentionunlist)
###it is possible that people type it in themselves so it might be a little different, therefore you need to make the characters lowercase
lowermention <- tolower(mentionunlist)
mentioncounts<-as.data.frame(table(lowermention))
colnames(mentioncounts)[1]<-"mention"
colnames(mentioncounts)[2]<-"frequency"
numberofuniquementions<-nrow(mentioncounts)
#mentioncounts<-sort(mentioncounts, decreasing=TRUE)
filename=paste("mentioncounts", inputfile)
write.csv(mentioncounts, file = filename, row.names=FALSE)
##################################################################################
##################################################################################
###PRINTS EDGELIST FOR REPLYTO STRUCTURE, the 3 and 6 are the column placements for the screename and to results
######NEED TO FIGURE OUT HOW TO GET RID OF NA's in the data
####12 is the column in the data we output
replytoedge <- df[c(5,10)]
replytoedge = na.omit(replytoedge)
colnames(replytoedge)[1]<-"source"
colnames(replytoedge)[2]<-"target"
#write.csv(replytoedge, file="replytoedgelist.csv", row.names=FALSE)
filename=paste("replytoedge", inputfile)
write.csv(replytoedge, file = filename, row.names=FALSE)

##################################################################################
##################################################################################
###PRINTS EDGELIST FOR FAUXTO STRUCTURE, 
######NEED TO FIGURE OUT HOW TO GET RID OF NA's in the data
####12 is the column in the data we output
fauxtoedge <- df[c(5,11)]
fauxtoedge = na.omit(fauxtoedge)
colnames(fauxtoedge)[1]<-"source"
colnames(fauxtoedge)[2]<-"target"
#write.csv(replytoedge, file="replytoedgelist.csv", row.names=FALSE)
filename=paste("fauxtoedge", inputfile)
write.csv(fauxtoedge, file = filename, row.names=FALSE)

##################################################################################

##################################################################################
###PRINTS EDGELIST FOR RETWEET STRUCTURE
retweetedge <- df[c(5,12)]
retweetedge = na.omit(retweetedge)
colnames(retweetedge)[1]<-"source"
colnames(retweetedge)[2]<-"target"
retweetedgetarget<-tolower(retweetedge$target)
#write.csv(retweetedge, file="retweetedgelist.csv", row.names=FALSE)replyto
filename=paste("retweetedge", inputfile)
write.csv(retweetedge, file = filename, row.names=FALSE)
##################################################################################
######RANK THE MOST RETWEETED INDIVIDUALS
mostretweeted<-table(retweetedge$target)
highestretweeters<-table(retweetedge$source)
##################

##################################################################################
##################################################################################
##################################################################################
####IDENTIFY IF A USERS ONLY TWEET IS A RETWEET

######get the users who only contributed once and then spit them out to a file and read back in for clarity
single<-as.data.frame(singleton)
write.csv(singleton, file="singleton.csv")
ty<-read.csv(file="singleton.csv")
single <- ty[c(1)]
singlecontributors<-single$X
####pull in retweeters from retweet edge
retweeters<-retweetedge$source
retweeters<-unique(retweeters)
####identify those individuals who participated once and were in the retweetedge$source
onlyretweet <-intersect(retweeters, singlecontributors)
###get length of those that only retweeted
onlyretweetlength<-length(onlyretweet)
####percentage of tweeters who only participated in the dataset by retweeting
retweetsingletonpercentage<-onlyretweetlength/numberofuniquetweeters
####percentage of single contributors to the dataset who participated by retweeting
overallsingletonpercentage<-onlyretweetlength/numberofsingleposters
##################################################################################

#####PULLS OUT LINKS FROM TWEET TEXT
###THE URL REGEX PULLS OUT ANYTHING FOLLOWING AN HTTP UNTIL THE NEXT WHITESPACE SO IT IS POSSIBLE THAT PEOPLE THAT DO NOT PUT A A SPACE AFTER LINK WOULD GET EXTRA CHARACTERS IN, THAT IS WHY THE STR.MATCH IS LEFT AS HTTP........
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
##################################################################################
##################################################################################
##################################################################################

#####CREATE A SUMMARY TABLE OF THE ABOVE PERCENTAGES AND WRITE TO FILE
syntacticfeatureoverview <- table(inputfile, numberoftweets, linkpresencepercentage, hashpresencepercentage, mentionpresencepercentage, replytopresencepercentage, rtpresencepercentage, viapresencepercentage, mtpresencepercentage, deviceduplicatepercentage, numberofdevices, multipledeviceusers, numberofpeopleretweeted, numberofuniquetweeters, numberofuniquehashtags, numberofuniquementions, numberofuniquelinks, uniquetweets, numberofsingleposters, singletonpercentage, percentofenglishtweets, geopresence, fauxtopresencepercentage, meantweetcharacterlength, retweetsingletonpercentage, multiplelanguageusers, obamapresencepercentage, bidenpresencepercentage, mittpresencepercentage, ryanpresencepercentage)

filename=paste("syntacticfeatureoverview", inputfile)
write.csv(syntacticfeatureoverview, file = filename, row.names=TRUE)

####transform these to counts for easier analysis

linkpresencepercentage<-numberoftweets * linkpresencepercentage
hashpresencepercentage<-numberoftweets * hashpresencepercentage
mentionpresencepercentage <-numberoftweets * mentionpresencepercentage
replytopresencepercentage <-numberoftweets * replytopresencepercentage 
rtpresencepercentage <-numberoftweets * rtpresencepercentage
viapresencepercentage<- numberoftweets * viapresencepercentage 
mtpresencepercentage <-numberoftweets * mtpresencepercentage 
deviceduplicatepercentage <-numberoftweets * deviceduplicatepercentage 
deviceduplicatepercentage <- round(deviceduplicatepercentage)
singletonpercentage <-numberoftweets * singletonpercentage
percentofenglishtweets<- numberoftweets * percentofenglishtweets
geopresence <-numberoftweets * geopresence
fauxtopresencepercentage <-numberoftweets * fauxtopresencepercentage
retweetsingletonpercentage <-numberoftweets * retweetsingletonpercentage
retweetsingletonpercentage<-round(retweetsingletonpercentage)
obamapresencepercentage <-numberoftweets * obamapresencepercentage
bidenpresencepercentage<- numberoftweets * bidenpresencepercentage 
mittpresencepercentage <-numberoftweets * mittpresencepercentage
ryanpresencepercentage <-numberoftweets * ryanpresencepercentage

###write counts to file
syntacticfeatureoverviewWHOLE <- table(inputfile, numberoftweets, linkpresencepercentage, hashpresencepercentage, mentionpresencepercentage, replytopresencepercentage, rtpresencepercentage, viapresencepercentage, mtpresencepercentage, deviceduplicatepercentage, numberofdevices, multipledeviceusers, numberofpeopleretweeted, numberofuniquetweeters, numberofuniquehashtags, numberofuniquementions, numberofuniquelinks, uniquetweets, numberofsingleposters, singletonpercentage, percentofenglishtweets, geopresence, fauxtopresencepercentage, meantweetcharacterlength, retweetsingletonpercentage, multiplelanguageusers, obamapresencepercentage, bidenpresencepercentage, mittpresencepercentage, ryanpresencepercentage)

filename=paste("syntacticfeatureoverviewWHOLE", inputfile)
write.csv(syntacticfeatureoverviewWHOLE, file = filename, row.names=TRUE)
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

##################################################################################
#####SIMPLE TIME SERIES
timeseriestable <- as.data.frame(table(df$created_at))
colnames(timeseriestable)[1]<-"time"
colnames(timeseriestable)[2]<-"tweets"
filename=paste("timeseries", inputfile)
write.csv(timeseriestable, file = filename, row.names=FALSE)


##################################################################################
##################################################################################

endtime<-Sys.time()
runtime=endtime-startime
runtimemessage<-paste("This took", runtime)
print(runtimemessage)

####change working dir for output
setwd("/Users/cmascaro/Desktop/Dissertation Time Periods/ScriptDev")

}

