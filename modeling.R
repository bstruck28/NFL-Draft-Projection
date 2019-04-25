#Modeling

library(randomForest)

rf.train.1 <- train[, c(2:11, 16:20)]

#in order to do a random forest model, there cannot be missing pick values, so we will assign a pick
#value of 261 to all missing pick values, since the max in the dataset is 260

train$Pick[is.na(train$Pick)] <- 261


rf.label <- as.numeric(train$Pick)

set.seed(2398)
rf <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000, mtry = 6)
rf
plot(rf)

test <- rbind(train[1, ] , test)
test <- test[-1,]

test.cols <- test[, c(2:11, 16:20)]

rf.preds <- predict(rf, test.cols)

test.df <- data.frame(rep(test), Predicted_Pick = rf.preds)

#since our predicted pick outputs don't have much variance, we will list them in numerical order by
#year and make a mock draft based on that

library(dplyr)

#use 2016 as a reference

preds2016test <- test.df %>%
  filter(Year == 2016) %>%
  arrange(Predicted_Pick) %>%
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)

#according to the 84 value sample, we correctly predicted 3 first round picks in this test sample

#No cross validation is needed for a random forest model

#Now let's do predictions for the whole dataset (train and test combined) and split up by year

data.combined <- rbind(train,test)

#in order to do a random forest model, there cannot be missing pick values, so we will assign a pick
#value of 261 to all missing pick values, since the max in the dataset is 260

data.combined$Pick[is.na(data.combined$Pick)] <- 261

data.combined.cols <- data.combined[, c(2:11, 16:20)]

rf.preds2 <- predict(rf, data.combined.cols)

test.df2 <- data.frame(rep(data.combined), Predicted_Pick = rf.preds2)

preds2000 <- test.df2 %>%
  filter(Year == 2000) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2000$Predicted_Pick <= 32 & preds2000$Pick <= 32) 

preds2001 <- test.df2 %>%
  filter(Year == 2001) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2001$Predicted_Pick <= 32 & preds2001$Pick <= 32) 

preds2002 <- test.df2 %>%
  filter(Year == 2002) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2002$Predicted_Pick <= 32 & preds2002$Pick <= 32) 

preds2003 <- test.df2 %>%
  filter(Year == 2003) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2003$Predicted_Pick <= 32 & preds2003$Pick <= 32)

preds2004 <- test.df2 %>%
  filter(Year == 2004) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2004$Predicted_Pick <= 32 & preds2004$Pick <= 32) 

preds2005 <- test.df2 %>%
  filter(Year == 2005) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2005$Predicted_Pick <= 32 & preds2005$Pick <= 32) 

preds2006 <- test.df2 %>%
  filter(Year == 2006) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%  
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2006$Predicted_Pick <= 32 & preds2006$Pick <= 32) 

preds2007 <- test.df2 %>%
  filter(Year == 2007) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2007$Predicted_Pick <= 32 & preds2007$Pick <= 32) 

preds2008 <- test.df2 %>%
  filter(Year == 2008) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2008$Predicted_Pick <= 32 & preds2008$Pick <= 32) 

preds2009 <- test.df2 %>%
  filter(Year == 2009) %>%
  arrange(Predicted_Pick) %>%                
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2009$Predicted_Pick <= 32 & preds2009$Pick <= 32) 

preds2010 <- test.df2 %>%
  filter(Year == 2010) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2010$Predicted_Pick <= 32 & preds2010$Pick <= 32) 

preds2011 <- test.df2 %>%
  filter(Year == 2011) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2011$Predicted_Pick <= 32 & preds2011$Pick <= 32) 

preds2012 <- test.df2 %>%
  filter(Year == 2012) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2012$Predicted_Pick <= 32 & preds2012$Pick <= 32) 

preds2013 <- test.df2 %>%
  filter(Year == 2013) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2013$Predicted_Pick <= 32 & preds2013$Pick <= 32) 

preds2014 <- test.df2 %>%
  filter(Year == 2014) %>%
  arrange(Predicted_Pick) %>%                
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2014$Predicted_Pick <= 32 & preds2014$Pick <= 32) 

preds2015 <- test.df2 %>%
  filter(Year == 2015) %>%
  arrange(Predicted_Pick) %>%                 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2015$Predicted_Pick <= 32 & preds2015$Pick <= 32) 

preds2016 <- test.df2 %>%
  filter(Year == 2016) %>%
  arrange(Predicted_Pick) %>%                
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2016$Predicted_Pick <= 32 & preds2016$Pick <= 32) 

preds2017 <- test.df2 %>%
  filter(Year == 2017) %>%
  arrange(Predicted_Pick) %>%                
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Pick, Player, Pos)
sum(preds2017$Predicted_Pick <= 32 & preds2017$Pick <= 32) 

#average of 21 out of 32 first round picks correctly identified! Impressive because the model does not
#take into account many factors such as school, team needs, off field issues, etc. just a straight
#prediction based on combine results

#Now let's test our model with the 2018 data

train_2018 <- read.csv('train_2018.csv')
test_2018 <- read.csv('test_2018.csv')

data_2018 <- rbind(train_2018, test_2018)
data_2018$X <- NULL

data_2018[which(data_2018$Pos == "CB"), "Pos"] <- "DB"
data_2018[which(data_2018$Pos == "FS"), "Pos"] <- "DB"
data_2018[which(data_2018$Pos == "SS"), "Pos"] <- "DB"
data_2018[which(data_2018$Pos == "S"), "Pos"] <- "DB"
data_2018[which(data_2018$Pos == "EDGE"), "Pos"] <- "DE"
data_2018[which(data_2018$Pos == "G"), "Pos"] <- "OL"
data_2018[which(data_2018$Pos == "C"), "Pos"] <- "OL"
data_2018[which(data_2018$Pos == "OG"), "Pos"] <- "OL"
data_2018[which(data_2018$Pos == "OT"), "Pos"] <- "OL"
data_2018[which(data_2018$Pos == "ILB"), "Pos"] <- "LB"
data_2018[which(data_2018$Pos == "OLB"), "Pos"] <- "LB"
data_2018[which(data_2018$Pos == "NT"), "Pos"] <- "DT"

str(data_2018$Pos)

data_2018.2 <- data_2018 #for re-adding columns after imputation

#Imputation, same as before with missForest

library(missForest)

set.seed(456)
data_2018 <- missForest(data_2018[, 3:10], maxiter = 10)
data_2018$OOBerror #error of 0.02875057 which is good
data_2018 <- data_2018[["ximp"]]

data_2018$Player <- data_2018.2$Player
data_2018$Pos <- data_2018.2$Pos
data_2018$Year <- data_2018.2$Year

data_2018 <- data_2018[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                         "Cone", "Shuttle", "Player", "Pos", "Year")]
data_2018 <- data_2018[c(9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 11)]

data_2018$Ht <- round(data_2018$Ht, 0)
data_2018$Wt <- round(data_2018$Wt, 0)
data_2018$Forty <- round(data_2018$Forty, 2)
data_2018$Vertical <- round(data_2018$Vertical, 0)
data_2018$BenchReps <- round(data_2018$BenchReps, 0)
data_2018$BroadJump <- round(data_2018$BroadJump, 0)
data_2018$Cone <- round(data_2018$Cone, 2)
data_2018$Shuttle <- round(data_2018$Shuttle, 2)

#feature engineering, same metrics as before

#BMI
data_2018$BMI <- (data_2018$Wt / data_2018$Ht ** 2) * 703

#create data frames for positions using 2019 data
OL_2018 <- data_2018[which(data_2018$Pos %in% "OL"),]
DB_2018 <- data_2018[which(data_2018$Pos %in% "DB"),]
DE_2018 <- data_2018[which(data_2018$Pos %in% "DE"),]
DT_2018 <- data_2018[which(data_2018$Pos %in% "DT"),]
FB_2018 <- data_2018[which(data_2018$Pos %in% "FB"),]
K_2018 <- data_2018[which(data_2018$Pos %in% "K"),]
LB_2018 <- data_2018[which(data_2018$Pos %in% "LB"),]
LS_2018 <- data_2018[which(data_2018$Pos %in% "LS"),]
P_2018 <- data_2018[which(data_2018$Pos %in% "P"),]
QB_2018 <- data_2018[which(data_2018$Pos %in% "QB"),]
RB_2018 <- data_2018[which(data_2018$Pos %in% "RB"),]
TE_2018 <- data_2018[which(data_2018$Pos %in% "TE"),]
WR_2018 <- data_2018[which(data_2018$Pos %in% "WR"),]

#Speed score
data_2018$SpeedScore <- (data_2018$Wt * 200) / (data_2018$Forty ** 4)  #Speed score rewards bigger players for being faster


#Height Adjusted Speed Score, rewards faster players who are taller, ifelse loop is going to be ugly
data_2018$HA_SpeedScore <-
  ifelse(
    (data_2018$Pos %in% "DB"),
    data_2018$SpeedScore * (data_2018$Ht / mean(DB_2018$Ht)),
    ifelse(
      (data_2018$Pos %in% "DE"),
      data_2018$SpeedScore * (data_2018$Ht / mean(DE_2018$Ht)),
      ifelse(
        (data_2018$Pos %in% "DT"),
        data_2018$SpeedScore * (data_2018$Ht / mean(DT_2018$Ht)),
        ifelse(
          (data_2018$Pos %in% "FB"),
          data_2018$SpeedScore * (data_2018$Ht / mean(FB_2018$Ht)),
          ifelse(
            (data_2018$Pos %in% "K"),
            data_2018$SpeedScore * (data_2018$Ht / mean(K_2018$Ht)),
            ifelse(
              (data_2018$Pos %in% "LB"),
              data_2018$SpeedScore * (data_2018$Ht / mean(LB_2018$Ht)),
              ifelse(
                (data_2018$Pos %in% "LS"),
                data_2018$SpeedScore * (data_2018$Ht / mean(LS_2018$Ht)),
                ifelse(
                  (data_2018$Pos %in% "OL"),
                  data_2018$SpeedScore * (data_2018$Ht / mean(OL_2018$Ht)),
                  ifelse(
                    (data_2018$Pos %in% "P"),
                    data_2018$SpeedScore * (data_2018$Ht / mean(P_2018$Ht)),
                    ifelse(
                      (data_2018$Pos %in% "QB"),
                      data_2018$SpeedScore * (data_2018$Ht / mean(QB_2018$Ht)),
                      ifelse(
                        (data_2018$Pos %in% "RB"),
                        data_2018$SpeedScore * (data_2018$Ht / mean(RB_2018$Ht)),
                        ifelse(
                          (data_2018$Pos %in% "TE"),
                          data_2018$SpeedScore * (data_2018$Ht / mean(TE_2018$Ht)),
                          ifelse(
                            (data_2018$Pos %in% "WR"),
                            data_2018$SpeedScore * (data_2018$Ht / mean(WR_2018$Ht)),
                            ""
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
data_2018$HA_SpeedScore <- as.numeric(data_2018$HA_SpeedScore)

#Agility score: sum of Shuttle and Cone drill times, lower score is better
data_2018$AgilityScore <- data_2018$Cone + data_2018$Shuttle

#Weight adjustedBurst Score: sum of Vertical and BroadJump, bigger score is better
data_2018$BurstScore <- data_2018$Vertical + data_2018$BroadJump

#Final metric is athleticism score, which aggregates Forty time with agility score and burst score, and
#rewards players with a higher BMI
data_2018$AthleticismScore <-
  ifelse(
    (data_2018$Pos %in% "DB"),
    (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(DB_2018$BMI)),
    ifelse(
      (data_2018$Pos %in% "DE"),
      (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(DE_2018$BMI)),
      ifelse(
        (data_2018$Pos %in% "DT"),
        (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(DT_2018$BMI)),
        ifelse(
          (data_2018$Pos %in% "FB"),
          (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(FB_2018$BMI)),
          ifelse(
            (data_2018$Pos %in% "K"),
            (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(K_2018$BMI)),
            ifelse(
              (data_2018$Pos %in% "LB"),
              (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(LB_2018$BMI)),
              ifelse(
                (data_2018$Pos %in% "LS"),
                (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(LS_2018$BMI)),
                ifelse(
                  (data_2018$Pos %in% "OL"),
                  (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(OL_2018$BMI)),
                  ifelse(
                    (data_2018$Pos %in% "P"),
                    (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(P_2018$BMI)),
                    ifelse(
                      (data_2018$Pos %in% "QB"),
                      (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(QB_2018$BMI)),
                      ifelse(
                        (data_2018$Pos %in% "RB"),
                        (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(RB_2018$BMI)),
                        ifelse(
                          (data_2018$Pos %in% "TE"),
                          (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(TE_2018$BMI)),
                          ifelse(
                            (data_2018$Pos %in% "WR"),
                            (data_2018$Forty + data_2018$AgilityScore + data_2018$BurstScore) * (data_2018$BMI / mean(WR_2018$BMI)),
                            ""
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
data_2018$AthleticismScore <- as.numeric(data_2018$AthleticismScore)

#Make predictions

str(data_2018)

data_2018$Pos <- factor(data_2018$Pos)
str(data_2018)

data_2018.cols <- data_2018[, c(2:11, 13:17)]


rf.preds3 <- predict(rf, data_2018.cols)

test.df3 <- data.frame(rep(data_2018), Predicted_Pick = rf.preds3)


preds2018 <- test.df3 %>%
  filter(Year == 2018) %>%
  arrange(Predicted_Pick) %>% 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Player, Pos)


#The reason for making this model was to use it for subsequent drafts and see how it does, so with the
#2019 draft in less than 2 weeks, lets get the 2019 data by doing some web scraping

library(rvest)

url <- "https://www.pro-football-reference.com/draft/2019-combine.htm"
webpage <- read_html(url)

#Get Player Name
Player <- html_nodes(webpage, "th a")
Player <- as.character(html_text(Player))

Player <- data.frame(Player, stringsAsFactors = FALSE)

#a few names are missing so let's add them in

library(tidyverse)

Player <- Player %>% add_row(Player = c("Nasir Adderley", "Ugo Amadi", "Corey Ballentine", "Jordan Brown",
                          "John Cominsky", "Xavier Crawford", "Jamal Davis", "Keelan Doss",
                          "Ashton Dulin", "Chauncey Gardner-Johnson", "Ethan Greenidge",
                          "Tytus Howard", "Jalen Hurd", "Darryl Johnson", "Devon Johnson",
                          "Joshua Miles", "Iosua Opeta", "Trey Pipkins", "Khalen Saunders",
                          "Max Scharping", "Sutton Smith", "Benny Snell", "Easton Stick",
                          "Derrek Thomas", "Oli Udoh", "Alex Wesley"))

#Re-arrange alphabetically by last name

#One player with the last name Van ginkel is sorted with the "g" last names on PFR, so I am going to
#change last name

Player[which(Player$Player == "Andrew Van ginkel"), "Player"] <- "Andrew ginkel Van"

Player <- Player %>%
  arrange(str_extract(Player,'\\s.*$'))

#Get Player Position
Pos <- html_nodes(webpage, "th+ td")
Pos <- as.character(html_text(Pos))

#Get Player Height
Ht <- html_nodes(webpage, ".right:nth-child(5)")
Ht <- as.character(html_text(Ht))

#Convert to inches
Ht <- sapply(strsplit(as.character(Ht),"-"),
       function(x){12*as.numeric(x[1]) + as.numeric(x[2])})

#Get Player Weight
Wt <- html_nodes(webpage, ".right:nth-child(6)")
Wt <- as.numeric(html_text(Wt))

#Get Player Forty Time
Forty <- html_nodes(webpage, ".right:nth-child(7)")
Forty <- as.numeric(html_text(Forty))

#Get Player Vertical
Vertical <- html_nodes(webpage, ".right:nth-child(8)")
Vertical <- as.numeric(html_text(Vertical))

#Get Player Bench Reps
BenchReps <- html_nodes(webpage, ".right:nth-child(9)")
BenchReps <- as.numeric(html_text(BenchReps))

#Get Player Broad Jump
BroadJump <- html_nodes(webpage, ".right:nth-child(10)")
BroadJump <- as.numeric(html_text(BroadJump))

#Get Player 3-cone time
Cone <- html_nodes(webpage, ".right:nth-child(11)")
Cone <- as.numeric(html_text(Cone))

#Get Player 20-yard shuttle time
Shuttle <- html_nodes(webpage, ".right:nth-child(12)")
Shuttle <- as.numeric(html_text(Shuttle))

#save all of the data to one data frame

data_2019 <- data.frame("Player" = Player, "Pos" = Pos, "Ht" = Ht, "Wt" = Wt, "Forty" = Forty,
                        "Vertical" = Vertical, "BenchReps" = BenchReps, "BroadJump" = BroadJump,
                        "Cone" = Cone, "Shuttle" = Shuttle, stringsAsFactors = FALSE)

data_2019$Year <- "2019"

#Re-assign positions like before

#For some reason #Pro Football Reference removed labeling participants as either DE or DT, so I need to
#reclassify the "DL" position by finding the average weight of the DE's and DT's in their respective
#datasets and set a cutoff

mean(DE$Wt) #[1] 268.2703
mean(DT$Wt) #[1] 306.7670


data_2019[which(data_2019$Pos == "CB"), "Pos"] <- "DB"
data_2019[which(data_2019$Pos == "FS"), "Pos"] <- "DB"
data_2019[which(data_2019$Pos == "SS"), "Pos"] <- "DB"
data_2019[which(data_2019$Pos == "S"), "Pos"] <- "DB"
data_2019[which(data_2019$Pos == "EDGE"), "Pos"] <- "DE"
data_2019[which(data_2019$Pos == "DL" & data_2019$Wt < 300), "Pos"] <- "DE"
data_2019[which(data_2019$Pos == "G"), "Pos"] <- "OL"
data_2019[which(data_2019$Pos == "C"), "Pos"] <- "OL"
data_2019[which(data_2019$Pos == "OG"), "Pos"] <- "OL"
data_2019[which(data_2019$Pos == "OT"), "Pos"] <- "OL"
data_2019[which(data_2019$Pos == "ILB"), "Pos"] <- "LB"
data_2019[which(data_2019$Pos == "OLB"), "Pos"] <- "LB"
data_2019[which(data_2019$Pos == "NT"), "Pos"] <- "DT"
data_2019[which(data_2019$Pos == "DL" & data_2019$Wt >= 300), "Pos"] <- "DT"

data_2019.2 <- data_2019 #for re-adding columns after imputation

#Imputation, same as before with missForest

library(missForest)

set.seed(456)
data_2019 <- missForest(data_2019[, 3:10], maxiter = 10)
data_2019$OOBerror #error of 0.02875057 which is good
data_2019 <- data_2019[["ximp"]]

data_2019$Player <- data_2019.2$Player
data_2019$Pos <- data_2019.2$Pos
data_2019$Year <- data_2019.2$Year

data_2019 <- data_2019[c("Ht", "Wt", "Forty", "Vertical", "BenchReps", "BroadJump",
                 "Cone", "Shuttle", "Player", "Pos", "Year")]
data_2019 <- data_2019[c(9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 11)]

data_2019$Ht <- round(data_2019$Ht, 0)
data_2019$Wt <- round(data_2019$Wt, 0)
data_2019$Forty <- round(data_2019$Forty, 2)
data_2019$Vertical <- round(data_2019$Vertical, 0)
data_2019$BenchReps <- round(data_2019$BenchReps, 0)
data_2019$BroadJump <- round(data_2019$BroadJump, 0)
data_2019$Cone <- round(data_2019$Cone, 2)
data_2019$Shuttle <- round(data_2019$Shuttle, 2)

#feature engineering, same metrics as before

#BMI
data_2019$BMI <- (data_2019$Wt / data_2019$Ht ** 2) * 703

#create data frames for positions using 2019 data
OL_2019 <- data_2019[which(data_2019$Pos %in% "OL"),]
DB_2019 <- data_2019[which(data_2019$Pos %in% "DB"),]
DE_2019 <- data_2019[which(data_2019$Pos %in% "DE"),]
DT_2019 <- data_2019[which(data_2019$Pos %in% "DT"),]
FB_2019 <- data_2019[which(data_2019$Pos %in% "FB"),]
K_2019 <- data_2019[which(data_2019$Pos %in% "K"),]
LB_2019 <- data_2019[which(data_2019$Pos %in% "LB"),]
LS_2019 <- data_2019[which(data_2019$Pos %in% "LS"),]
P_2019 <- data_2019[which(data_2019$Pos %in% "P"),]
QB_2019 <- data_2019[which(data_2019$Pos %in% "QB"),]
RB_2019 <- data_2019[which(data_2019$Pos %in% "RB"),]
TE_2019 <- data_2019[which(data_2019$Pos %in% "TE"),]
WR_2019 <- data_2019[which(data_2019$Pos %in% "WR"),]

#Speed score
data_2019$SpeedScore <- (data_2019$Wt * 200) / (data_2019$Forty ** 4)  #Speed score rewards bigger players for being faster


#Height Adjusted Speed Score, rewards faster players who are taller, ifelse loop is going to be ugly
data_2019$HA_SpeedScore <-
  ifelse(
    (data_2019$Pos %in% "DB"),
    data_2019$SpeedScore * (data_2019$Ht / mean(DB_2019$Ht)),
    ifelse(
      (data_2019$Pos %in% "DE"),
      data_2019$SpeedScore * (data_2019$Ht / mean(DE_2019$Ht)),
      ifelse(
        (data_2019$Pos %in% "DT"),
        data_2019$SpeedScore * (data_2019$Ht / mean(DT_2019$Ht)),
        ifelse(
          (data_2019$Pos %in% "FB"),
          data_2019$SpeedScore * (data_2019$Ht / mean(FB_2019$Ht)),
          ifelse(
            (data_2019$Pos %in% "K"),
            data_2019$SpeedScore * (data_2019$Ht / mean(K_2019$Ht)),
            ifelse(
              (data_2019$Pos %in% "LB"),
              data_2019$SpeedScore * (data_2019$Ht / mean(LB_2019$Ht)),
              ifelse(
                (data_2019$Pos %in% "LS"),
                data_2019$SpeedScore * (data_2019$Ht / mean(LS_2019$Ht)),
                ifelse(
                  (data_2019$Pos %in% "OL"),
                  data_2019$SpeedScore * (data_2019$Ht / mean(OL_2019$Ht)),
                  ifelse(
                    (data_2019$Pos %in% "P"),
                    data_2019$SpeedScore * (data_2019$Ht / mean(P_2019$Ht)),
                    ifelse(
                      (data_2019$Pos %in% "QB"),
                      data_2019$SpeedScore * (data_2019$Ht / mean(QB_2019$Ht)),
                      ifelse(
                        (data_2019$Pos %in% "RB"),
                        data_2019$SpeedScore * (data_2019$Ht / mean(RB_2019$Ht)),
                        ifelse(
                          (data_2019$Pos %in% "TE"),
                          data_2019$SpeedScore * (data_2019$Ht / mean(TE_2019$Ht)),
                          ifelse(
                            (data_2019$Pos %in% "WR"),
                            data_2019$SpeedScore * (data_2019$Ht / mean(WR_2019$Ht)),
                            ""
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
data_2019$HA_SpeedScore <- as.numeric(data_2019$HA_SpeedScore)

#Agility score: sum of Shuttle and Cone drill times, lower score is better
data_2019$AgilityScore <- data_2019$Cone + data_2019$Shuttle

#Weight adjustedBurst Score: sum of Vertical and BroadJump, bigger score is better
data_2019$BurstScore <- data_2019$Vertical + data_2019$BroadJump

#Final metric is athleticism score, which aggregates Forty time with agility score and burst score, and
#rewards players with a higher BMI
data_2019$AthleticismScore <-
  ifelse(
    (data_2019$Pos %in% "DB"),
    (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(DB_2019$BMI)),
    ifelse(
      (data_2019$Pos %in% "DE"),
      (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(DE_2019$BMI)),
      ifelse(
        (data_2019$Pos %in% "DT"),
        (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(DT_2019$BMI)),
        ifelse(
          (data_2019$Pos %in% "FB"),
          (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(FB_2019$BMI)),
          ifelse(
            (data_2019$Pos %in% "K"),
            (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(K_2019$BMI)),
            ifelse(
              (data_2019$Pos %in% "LB"),
              (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(LB_2019$BMI)),
              ifelse(
                (data_2019$Pos %in% "LS"),
                (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(LS_2019$BMI)),
                ifelse(
                  (data_2019$Pos %in% "OL"),
                  (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(OL_2019$BMI)),
                  ifelse(
                    (data_2019$Pos %in% "P"),
                    (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(P_2019$BMI)),
                    ifelse(
                      (data_2019$Pos %in% "QB"),
                      (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(QB_2019$BMI)),
                      ifelse(
                        (data_2019$Pos %in% "RB"),
                        (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(RB_2019$BMI)),
                        ifelse(
                          (data_2019$Pos %in% "TE"),
                          (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(TE_2019$BMI)),
                          ifelse(
                            (data_2019$Pos %in% "WR"),
                            (data_2019$Forty + data_2019$AgilityScore + data_2019$BurstScore) * (data_2019$BMI / mean(WR_2019$BMI)),
                            ""
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
data_2019$AthleticismScore <- as.numeric(data_2019$AthleticismScore)

#Make predictions

data_2019$Pos <- as.factor(data_2019$Pos)

data_2019.cols <- data_2019[, c(2:11, 13:17)]


rf.preds4 <- predict(rf, data_2019.cols)

test.df4 <- data.frame(rep(data_2019), Predicted_Pick = rf.preds4)


preds2019 <- test.df4 %>%
  filter(Year == 2019) %>%
  arrange(Predicted_Pick) %>% 
  mutate(Predicted_Pick = row_number()) %>%
  select(Predicted_Pick, Player, Pos)
