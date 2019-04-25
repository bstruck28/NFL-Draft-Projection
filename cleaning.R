#dataset taken from Kaggle.com

setwd("~/nfl-combine-data")

combine_data <- read.csv('combine_data_since_2000_PROCESSED_2018-04-26.csv')

#split data into train and test sets

library(caTools)

sample <- sample.split(combine_data, SplitRatio = .75, set.seed(101))
train <- subset(combine_data, sample == TRUE)

#Since draft data for 2018 is missing, we will exclude such entries and use it to test our model later

train_2018 <- train[train$Year == "2018", ]
train <- train[train$Year != "2018", ]
test <-  subset(combine_data, sample == FALSE)
test_2018 <- test[test$Year == "2018", ]
test <- test[test$Year != "2018", ]

write.csv(train_2018, file = 'train_2018.csv')           #for predictions later
write.csv(test_2018, file = 'test_2018.csv')


str(train)
str(test)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

table(train$Forty)
table(train$Pos)

#Let's remove the AV column, because that is a predictor on how well a player will actually do
#in the NFL, while this model is only predicting draft position, and the Pfr_ID column, because
#it is not necessary, and do some data cleaning

train$AV <- NULL
train$Pfr_ID <- NULL

#since some positions are interchangeable, (e.g. EDGE to DE, G, OG, and OT to OL, S, FS, SS, 
#and CB to DB, etc.), let's do some more data cleaning

train[which(train$Pos == "CB"), "Pos"] <- "DB"
train[which(train$Pos == "FS"), "Pos"] <- "DB"
train[which(train$Pos == "SS"), "Pos"] <- "DB"
train[which(train$Pos == "S"), "Pos"] <- "DB"
train[which(train$Pos == "EDGE"), "Pos"] <- "DE"
train[which(train$Pos == "G"), "Pos"] <- "OL"
train[which(train$Pos == "C"), "Pos"] <- "OL"
train[which(train$Pos == "OG"), "Pos"] <- "OL"
train[which(train$Pos == "OT"), "Pos"] <- "OL"
train[which(train$Pos == "ILB"), "Pos"] <- "LB"
train[which(train$Pos == "OLB"), "Pos"] <- "LB"
train[which(train$Pos == "NT"), "Pos"] <- "DT"

train$Team <- as.character(train$Team)
train[which(train$Team == ""), "Team"] <- "Undrafted"

#create separate data frame to replace missing columns after using imputation procedures later on
train_2018_removed <- train
test_2018_removed <- test
