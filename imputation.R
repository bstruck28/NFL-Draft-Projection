#Since there are many missing values, we will use imputation, and test five different methods to see 
#which one is more accurate by creating random missing values

library(mice)

train <- as.numeric(train[, 3:10])
str(train)

#generate 10% missing values at random

library(missForest)

set.seed(123)

train.mis_mice <- prodNA(train[, 3:10], noNA = 0.1)
summary(train.mis_mice)
md.pattern(train.mis_mice)

library(VIM)

aggr(train.mis_mice, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

train.imputed_mice <- mice(train.mis_mice[,1:8], maxit = 50, method = 'pmm', seed = 123) #replication
summary(train.imputed_mice)

#Look at a sample of our imputed values

train.imputed_mice$imp$Ht

#load lattice package for visualization of imputed values

library(lattice)

xyplot(train.imputed_mice, Forty ~ Vertical+BenchReps+BroadJump+Cone+Shuttle, pch=18, cex = 1)
densityplot(train.imputed_mice)
stripplot(train.imputed_mice, pch=20, cex=1.2)

#looks very good! Now let's apply the obtained imputed values to our original 
# 'train.mis_mice' data frame

train.mis_mice <- complete(train.imputed_mice)

#Re-adding columns (reason mentioned in line 60)

train.mis_mice$Player <- train_2018_removed$Player
train.mis_mice$Pos <- train_2018_removed$Pos
train.mis_mice$Year <- train_2018_removed$Year
train.mis_mice$Team <- train_2018_removed$Team
train.mis_mice$Round <- train_2018_removed$Round
train.mis_mice$Pick <- train_2018_removed$Pick

#reorder columns to original

train.mis_mice <- train.mis_mice[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                               "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train.mis_mice <- train.mis_mice[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#Now lets try imputation with Amelia package

library(Amelia)

set.seed(1234) #replication

train.mis_Amelia <- prodNA(train[, 3:10], noNA = 0.1)
train.imputed_Amelia <- amelia(train.mis_Amelia[, 1:8], m=5)
train.mis_Amelia <- train.imputed_Amelia$imputations[[3]]

#Re-adding columns (reason mentioned in line 60)

train.mis_Amelia$Player <- train_2018_removed$Player
train.mis_Amelia$Pos <- train_2018_removed$Pos
train.mis_Amelia$Year <- train_2018_removed$Year
train.mis_Amelia$Team <- train_2018_removed$Team
train.mis_Amelia$Round <- train_2018_removed$Round
train.mis_Amelia$Pick <- train_2018_removed$Pick

#reorder columns to original

train.mis_Amelia <- train.mis_Amelia[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                                   "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train.mis_Amelia <- train.mis_Amelia[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#Now lets try imputation with missForest package

library(missForest)

set.seed(12345) #replication

train.mis_missForest <- prodNA(train[, 3:10], noNA = 0.1)
train.imputed_missForest <- missForest(train.mis_missForest[, 1:8])
train.imputed_missForest$OOBerror #error of 0.05812961, which is good
train.mis_missForest <- train.imputed_missForest[["ximp"]]


#Re-adding columns (reason mentioned in line 60)

train.mis_missForest$Player <- train_2018_removed$Player
train.mis_missForest$Pos <- train_2018_removed$Pos
train.mis_missForest$Year <- train_2018_removed$Year
train.mis_missForest$Team <- train_2018_removed$Team
train.mis_missForest$Round <- train_2018_removed$Round
train.mis_missForest$Pick <- train_2018_removed$Pick

#reorder columns to original

train.mis_missForest <- train.mis_missForest[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                                       "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train.mis_missForest <- train.mis_missForest[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#And now for Hmisc

library(Hmisc)

set.seed(987)

train.mis_Hmisc <- prodNA(train[, 3:10], noNA = 0.1)
train.imputed_Hmisc <- aregImpute(~ Ht + Wt + Forty + Vertical + BenchReps + BroadJump +
                              Cone + Shuttle, data = train.mis_Hmisc[, 1:8], n.impute = 5)
train.mis_Hmisc <-impute.transcan(train.imputed_Hmisc, data=train.mis_Hmisc, 
                                  imputation=1, list.out=TRUE, pr=FALSE, check=FALSE)
write.csv(train.mis_Hmisc, file = "train.mis_Hmisc.csv")
train.mis_Hmisc <- read.csv('train.mis_Hmisc.csv', header = TRUE)
train.mis_Hmisc$X <- NULL

#Re-adding columns (reason mentioned in line 60)

train.mis_Hmisc$Player <- train_2018_removed$Player
train.mis_Hmisc$Pos <- train_2018_removed$Pos
train.mis_Hmisc$Year <- train_2018_removed$Year
train.mis_Hmisc$Team <- train_2018_removed$Team
train.mis_Hmisc$Round <- train_2018_removed$Round
train.mis_Hmisc$Pick <- train_2018_removed$Pick

#reorder columns to original

train.mis_Hmisc <- train.mis_Hmisc[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                                               "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train.mis_Hmisc <- train.mis_Hmisc[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#And finally, the mi package

library(mi)

set.seed(987)

train.mis_mi <- prodNA(train[, 3:10], noNA = 0.1)
train.imputed_mi <- mi(train.mis_mi, seed = 987)
train.mis_mi <- complete(train.imputed_mi)
write.csv(train.mis_mi, file = "train.mis_mi.csv")

library(readxl)

train.mis_mi <- read_excel("train.mis_mi.xlsx", sheet = 2)
train.mis_mi$...1 <- NULL

#Re-adding columns (reason mentioned in line 60)

train.mis_mi$Player <- train_2018_removed$Player
train.mis_mi$Pos <- train_2018_removed$Pos
train.mis_mi$Year <- train_2018_removed$Year
train.mis_mi$Team <- train_2018_removed$Team
train.mis_mi$Round <- train_2018_removed$Round
train.mis_mi$Pick <- train_2018_removed$Pick

#reorder columns to original

train.mis_mi <- train.mis_mi[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                                     "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train.mis_mi <- train.mis_mi[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#To find the best models, lets find the correlations between the original train data set
#and the completed missing data sets (where 10% of values were generated as missing at random)

cor_mice <- cor(train.mis_mice[, 3:10], train[, 3:10], use = 'complete.obs')
cor_amelia <- cor(train.mis_Amelia[, 3:10], train[, 3:10], use = 'complete.obs')
cor_missForest <- cor(train.mis_missForest[, 3:10], train[, 3:10], use = 'complete.obs')
cor_Hmisc <- cor(train.mis_Hmisc[, 3:10], train[, 3:10], use = 'complete.obs')
cor_mi <- cor(train.mis_mi[, 3:10], train[, 3:10], use = 'complete.obs')

#we only want to look at the correlations between the same variables for the separate data frames,
#for each variable and choose the largest one for each respective variable

#eyeballing, the best models to use for respective variables are:
#Ht: cor_missForest (0.9799372)
#Wt: cor_missForest (0.9958646)
#Forty: cor_missForest (0.9921644)
#Vertical: cor_missForest (0.9785248)
#BenchReps: cor_missForest (0.9770131)
#BroadJump: cor_mi (0.9877104)
#Cone: cor_missForest (0.9892757)
#Shuttle: cor_MissForest (0.9855440)

#As we can see, missForest was definitely our best imputation model, so we will use missForest
#without generating any missing values to keep all of the data we have

set.seed(456)
train <- missForest(train[, 3:10])
train$OOBerror #error of 0.02865355 which is good
train <- train[["ximp"]]
cor(train.mis_missForest[, 3:10], train[, 1:8]) #checking similarity

#Re-adding columns (reason mentioned in line 60)

train$Player <- train_2018_removed$Player
train$Pos <- train_2018_removed$Pos
train$Year <- train_2018_removed$Year
train$Team <- train_2018_removed$Team
train$Round <- train_2018_removed$Round
train$Pick <- train_2018_removed$Pick

#reorder columns to original

train <- train[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                               "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
train <- train[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#now lets round our values and get them in an acceptable format

train$Ht <- round(train$Ht, 0)
train$Wt <- round(train$Wt, 0)
train$Forty <- round(train$Forty, 2)
train$Vertical <- round(train$Vertical, 0)
train$BenchReps <- round(train$BenchReps, 0)
train$BroadJump <- round(train$BroadJump, 0)
train$Cone <- round(train$Cone, 2)
train$Shuttle <- round(train$Shuttle, 2)

#Repeat for the test set

set.seed(1234)
test <- missForest(test[, 3:10])
test$OOBerror #error of 0.02932602 which is good
test <- test[["ximp"]]

#Re-adding columns (reason mentioned in line 60)

test$Player <- test_2018_removed$Player
test$Pos <- test_2018_removed$Pos
test$Year <- test_2018_removed$Year
test$Team <- test_2018_removed$Team
test$Round <- test_2018_removed$Round
test$Pick <- test_2018_removed$Pick

#reorder columns to original

test <- test[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                 "Cone", "Shuttle", "Player", "Year", "Team", "Round", "Pick", "Pos")]
test <- test[c(9, 14, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)]

#now lets round our values and get them in an acceptable format

test$Ht <- round(test$Ht, 0)
test$Wt <- round(test$Wt, 0)
test$Forty <- round(test$Forty, 2)
test$Vertical <- round(test$Vertical, 0)
test$BenchReps <- round(test$BenchReps, 0)
test$BroadJump <- round(test$BroadJump, 0)
test$Cone <- round(test$Cone, 2)
test$Shuttle <- round(test$Shuttle, 2)

#rewrite csv files to include imputed data

train <- write.csv(train, file = "train.csv")

test <- write.csv(test, file = "test.csv")

#this concludes the imputation process!
