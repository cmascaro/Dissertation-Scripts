###########################################################################################################################################
###########################################################################################################################################
##############This script includes a function from Tony Breyal#############################################################################
##############http://tonybreyal.wordpress.com/2011/12/13/unshorten-any-url-created-using-url-shortening-services-decode_shortened_url/#####
##############This script decodes a shortened URL and iterates through three times to identify the original URL############################
##############The script then aggregates these URLs based on the base URL (e.g. cnn.com) and outputs a table of these URLs#################
###########################################################################################################################################
# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.
####Code below is adapted from Tony Breyal

decode_short_url <- function(url, ...) {
  # PACKAGES #
  require(RCurl)
  
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) )
    if(inherits(x, 'try-error') | length(grep(".*Location: (\\S+).*", x))<1) {
      return(u)
    } else {
      return(gsub(".*Location: (\\S+).*", "\\1", x))
    }
  }
  
  # MAIN #
  # return decoded URLs
  urls <- c(url, ...)
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}


#####################
#CODE BELOW IS COMPLETELY WRITTEN BY CHRISTOPHER	

library(stringr)
####read in file
link<-read.csv(file="linkcountRETWEET.csv")
inputfile<-link
###make link list a vector

#####trim possible punctuation at the end of the link
trimcolon <- function (x) sub(':$','',x)
trimcomma <- function (x) sub(',$','',x)
trimquestion<-function(x) sub('\\?$', '', x)
trimperiod<-function(x) sub('\\.$', '', x)
###apply trim functions
link$link<-trimperiod(trimcolon(trimcomma(trimquestion(link$link))))
###assign the links to a new vector for easier management

newlink<-as.vector(link$link)

###decode the link the first time
for (i in 1:length(newlink))
{
link$decode[i]=decode_short_url(newlink[i])
print(c(i))
}

####since a lot of twitter links are shortened twice, run it through another time
secondlink<-as.vector(link$decode)

for (i in 1:length(secondlink))
{
link$decodemore[i]=decode_short_url(secondlink[i])
print(c(i))
}

####transform the decoded links to a character and then identify the base link for analysis

thirdlink<-as.character(link$decodemore)

for (i in 1:length(thirdlink))
{
link$base[i]=str_extract(thirdlink[i], '(http://|https://).*?(/)')
}

####transforms links to matrix for easier management
link1<-as.matrix(link)
###write links to file
write.csv(link1, file="linkcountsDECODED.csv")

####generate table of the base links and print to file
link<-as.matrix(link)
link1<-as.data.frame(link)
link1$base=sapply(link1$base,function(row) iconv(row,to='UTF-8'))

linkanalysis<-table((link1$base))
write.csv(linkanalysis, file="linkbasetable.csv")

