#################################
#Data Mining Prject    04/25/18 #
#Data sets: steamspy api        #
#################################

#clear memory
rm(list=ls())

#install packages
install.packages(c("httr","jsonlite","xml2","car", "lubridate","factoextra"))
install.packages(c("e1071", "caret", "quanteda","irlba", "randomForest"))
library(xml2)
library(httr)
library(jsonlite)
library(lubridate)
library(car)
library(data.table)
library(plyr)
library(ggplot2)
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)


#change the directory to data mining folder
#setwd("/Users/jakekearns/Documents/NEW FILE DIRECTORY/School/Data Mining")
setwd("D:/NEW FILE DIRECTORY/School/Data Mining")

#bring in data
mydata = read.csv("MyData.csv")
other_data = read.csv("games-features.csv")

#Change name
names(other_data)[names(other_data) == 'QueryID'] <- 'appid'

#pull cols needed
needed_data = other_data[,c(1,44:56)]

#merge on appid - id for each game
combined = merge(mydata,needed_data, by="appid")
combined[2] = NULL

#check for nas
sum(is.na(combined$year))

#change logical to binary values (1- TRUE, 0 -FALSE)
combined[,24:36] <- lapply(combined[,24:36], as.numeric)
head(combined)

#start of analysis
summary(combined)

#Top games rated by # of owners
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

# #number of games by genre  ################## Notes (4/30/18): needs a transformation to label what type of game it is in one row
# ggplot(combined_reduced,aes(x=developer, y=average_forever)) + 
#   geom_histogram(stat = "identity") +
#   theme(axis.text.x=element_text(angle=45,hjust=1))


#ggplot(combined_reduced, aes(type, mean(userscore), color = type)) + geom_point()



#Number of games over time
combined$month_year = paste(combined$month,"-", combined$year) 
combined = na.omit(combined)
dates = count(combined, vars=c("year"))
dates = na.omit(dates)
dates = dates[8:27,]


#Variable selection with stepwise
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/
combined_reduced = combined[,c(3:17,19,24:36)]

combined_reduced$type = ifelse(combined_reduced$GenreIsNonGame == 1, 'Nongame',
                               ifelse(combined_reduced$GenreIsIndie == 1 , 'Indie',
                                      ifelse(combined_reduced$GenreIsAction == 1 , 'Action',
                                             ifelse(combined_reduced$GenreIsAdventure == 1,'Adventure',
                                                    ifelse(combined_reduced$GenreIsCasual == 1 , 'Casual',
                                                           ifelse(combined_reduced$GenreIsStrategy == 1 , 'Strat',
                                                                  ifelse(combined_reduced$GenreIsRPG == 1, 'RPG',
                                                                         ifelse(combined_reduced$GenreIsSimulation == 1, 'Simulation',
                                                                                ifelse(combined_reduced$GenreIsEarlyAccess == 1, 'Early Access',
                                                                                       ifelse(combined_reduced$GenreIsFreeToPlay == 1 ,'FTP', 
                                                                                              ifelse(combined$GenreIsSports == 1,'Sports',
                                                                                                     ifelse(combined_reduced$GenreIsRacing == 1, 'Racing',
                                                                                                            ifelse(combined_reduced$GenreIsMassivelyMultiplayer == 1, 'Multiplayer', 'Shit..')))))))))))))



combined_reduced_types = combined_reduced[,c(1:16,30)]
View(combined_reduced_types)
#creating a label 
combined_reduced_types$Label = ifelse(combined_reduced$score_rank>=80, "Buy","Dont buy")

################## Clustering #########################
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

#k means





##Gini index
#install.packages("ineq")
library(ineq)
ineq(combined_reduced_types[,8:15],type="Gini")

##################### variable selection ######################## 
install.packages("leaps")
library(leaps)
leaps = regsubsets(score_rank~.,data = combined_reduced_types, nbest=10, really.big = T)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

library(olsrr)
null = lm(score_rank~1, data=combined_reduced_types)
full = lm(score_rank~., data=combined_reduced_types)
#forward
step(null, scope = list(lower=null, upper=full), direction ="forward")
#backward
step(full, direction ="backward")
#stepwise
step(null, scope = list(upper=full), direction ="both")

######################### Multiple Linear regression ##########################
f = lm(score_rank ~ developer + players_2weeks_variance + 
         negative + positive + PriceFinal + players_2weeks +
         players_forever_variance
         ,data = combined_reduced_types)

#summary
summary(f)

#Analysis of Variance 
anova(f)

#Probability plot
f_probabilityplot = rstandard(f)
plot(f_probabilityplot)

qqnorm(f_probabilityplot, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Steamed Data") 
qqline(f_probabilityplot)

#residual plot #Note (5/1/18): Do a transformation to fix res
plot(f$fitted,f$residuals, xlab = "fitted y_i hat", ylab = "Residuals")

#partial regression plot
library(car)
avPlots(f)

#load package to get student residues 
library(MASS)
#get the student residuals 
studres(f)

#################### Machine learning ########################
# load libraries
library(caret)
library(rpart)

#10 fold cross validation 
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
combined_reduced_types = na.omit(combined_reduced_types)

#first model - going by score rank
model<- train(score_rank ~ developer + players_2weeks_variance + 
                negative + positive + PriceFinal + players_2weeks +
                players_forever_variance
              , data = train, trControl=train_control, method="rpart")



results = model$pred
View(results)

#Accurary 
model$results
#confusionMatrix(model$, combined_reduced$score_rank)

#second model - going by label
model2<- train(Label ~  players_2weeks_variance + 
                 negative + positive + PriceFinal + players_2weeks +
                 players_forever_variance
               , data = combined_reduced_types, trControl=train_control, method="rpart")

results2 = model2$pred
View(results2)

#Accurary 
model2$results
