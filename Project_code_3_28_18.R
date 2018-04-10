#################################
#Data Mining Prject    03/28/18 #
#Data sets: steamspy api        #
#################################

#using as a guide: 
# https://tclavelle.github.io/blog/r_and_apis/
# https://github.com/mas-ude/steam-data-stats/blob/master/steamAPIdata.R

#column headers and meanings
#   #appid - Steam Application ID. If it's 999999, then data for this application is hidden on developer's request, sorry.
#   #name - the game's name
#   #developer - comma separated list of the developers of the game
#   publisher - comma separated list of the publishers of the game
#   #score_rank - score rank of the game based on user reviews
#   #positive - number of positive user reviews
#   #negative - number of negative user reviews
#   #userscore - user score of the game
#   #owners - owners of this application on Steam.   Beware of free weekends!  
#   #owners_variance - variance in owners. The real number of owners lies somewhere on owners +/- owners_variance range.   
#   #players_forever - people that have played this game since March 2009.
#   #players_forever_variance - variance for total players.
#   #players_2weeks - people that have played this game in the last 2 weeks.
#   #players_2weeks_variance - variance for the number of players in the last two weeks. 
#   #average_forever - average playtime since March 2009. In minutes.
#   #average_2weeks - average playtime in the last two weeks. In minutes.
#   #median_forever - median playtime since March 2009. In minutes.
#   #median_2weeks - median playtime in the last two weeks. In minutes.
#   price - US price in cents.


#clear memory
rm(list=ls())

#install packages
install.packages(c("httr","jsonlite","xml2","car", "lubridate"))
library(xml2)
library(httr)
library(jsonlite)
library(lubridate)
library(car)
library(data.table)
library(ggplot2)

#change the directory to data mining folder
setwd = ("D:/NEW FILE DIRECTORY/School/Data Mining")

#location of data
url = "http://steamspy.com/api.php?request=all"
path = "http://steamspy.com/api.php"

#pulling in data
mydata = fromJSON("http://steamspy.com/api.php?request=all")
# new_data = lapply(mydata, function(x) {
#   x[sapply(x, is.null)] <- NA
#   list(x)
# })

#Need to find a way to get rid of nulls while pulling them into a dataframe = see example below
str(mydata$`524440`)

d <- data.frame()
for (i in mydata) {
  tmp <- data.frame(appid=i$appid, name=i$name, 
                    developer = i$developer,
                    #publisher = i$publisher, #throws error because a few are null
                    score_rank=i$score_rank,positive=i$positive,negative=i$negative,userscore=i$userscore,
                    owners=i$owners,owners_variance=i$owners_variance,
                    players_forever=i$players_forever,
                    players_forever_variance=i$players_forever_variance,
                    players_2weeks=i$players_2weeks, 
                    players_2weeks_variance=i$players_2weeks_variance, 
                    average_forever=i$average_forever, average_2weeks=i$average_2weeks, 
                    median_forever=i$median_forever, median_2weeks=i$median_2weeks)
                    #price =i$price) #throws error since a few are null
      
  d <- rbind(d, tmp)  
}

#Bring in price and release data
price_releaseDate = read.csv('/Users/jakekearns/Documents/NEW FILE DIRECTORY/School/Data Mining/games-features.csv')

#changing QueryID to appid
names(price_releaseDate)[names(price_releaseDate) == 'QueryID'] <- 'appid'

#merge on appid - id for each game
combined = merge(d,price_releaseDate[,c(1,5,59)], by="appid")

#fixing date column
combined$month = sapply(combined$ReleaseDate, substring, 1,3)
combined$year = sapply(combined$ReleaseDate, substring, 7,11)
combined$day =  sapply(combined$ReleaseDate, substring, 4,6)
combined$year = as.numeric(combined$year)
combined$day = as.numeric(combined$day)
combined$month = match(combined$month,month.abb)

#combining year and month
combined$year_month_day = paste(combined$year,"-", combined$month,"-", combined$day)
combined$year_month_day = ymd(combined$year_month_day)

#start of analysis
summary(combined)

# Top games rated by # of owners
options(scipen=6)
combined_ordered = combined[order(-combined$owners),]
ggplot(combined_ordered[1:5,],aes(x=name, y=owners)) + 
  geom_histogram(stat = "identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#average forever by deveopler
combined_ordered = combined[order(-combined$average_forever),]
ggplot(combined_ordered[1:10,],aes(x=developer, y=average_forever)) + 
  geom_histogram(stat = "identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Number of games over time


#regression analysis




