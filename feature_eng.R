#=================================================================================================
#As we can see, this data is highly non-linear and may have some difficulty predicting draft
#position on it's own, so let's do some feature engineering to try and make the final model better
#=================================================================================================

train <- read.csv('train.csv', header = TRUE)
train$X <- NULL
train$Pick <- as.numeric(train$Pick)

table(train$Pos)

#The final metric, athleticism score, will need BMI for the calculation later
train$BMI <- (train$Wt / train$Ht ** 2) * 703

OL <- train[which(train$Pos %in% "OL"),]
DB <- train[which(train$Pos %in% "DB"),]
DE <- train[which(train$Pos %in% "DE"),]
DT <- train[which(train$Pos %in% "DT"),]
FB <- train[which(train$Pos %in% "FB"),]
K <- train[which(train$Pos %in% "K"),]
LB <- train[which(train$Pos %in% "LB"),]
LS <- train[which(train$Pos %in% "LS"),]
P <- train[which(train$Pos %in% "P"),]
QB <- train[which(train$Pos %in% "QB"),]
RB <- train[which(train$Pos %in% "RB"),]
TE <- train[which(train$Pos %in% "TE"),]
WR <- train[which(train$Pos %in% "WR"),]

#I am going to do some feature engineering and add various metrics from playerprofiler.com

#Speed score
train$SpeedScore <- (train$Wt * 200) / (train$Forty ** 4)  #Speed score rewards bigger players for
#being faster

#Height Adjusted Speed Score, rewards faster players who are taller, ifelse loop is going to be ugly
train$HA_SpeedScore <-
  ifelse(
    (train$Pos %in% "DB"),
    train$SpeedScore * (train$Ht / mean(DB$Ht)),
    ifelse(
      (train$Pos %in% "DE"),
      train$SpeedScore * (train$Ht / mean(DE$Ht)),
      ifelse(
        (train$Pos %in% "DT"),
        train$SpeedScore * (train$Ht / mean(DT$Ht)),
        ifelse(
          (train$Pos %in% "FB"),
          train$SpeedScore * (train$Ht / mean(FB$Ht)),
          ifelse(
            (train$Pos %in% "K"),
            train$SpeedScore * (train$Ht / mean(K$Ht)),
            ifelse(
              (train$Pos %in% "LB"),
              train$SpeedScore * (train$Ht / mean(LB$Ht)),
              ifelse(
                (train$Pos %in% "LS"),
                train$SpeedScore * (train$Ht / mean(LS$Ht)),
                ifelse(
                  (train$Pos %in% "OL"),
                  train$SpeedScore * (train$Ht / mean(OL$Ht)),
                  ifelse(
                    (train$Pos %in% "P"),
                    train$SpeedScore * (train$Ht / mean(P$Ht)),
                    ifelse(
                      (train$Pos %in% "QB"),
                      train$SpeedScore * (train$Ht / mean(QB$Ht)),
                      ifelse(
                        (train$Pos %in% "RB"),
                        train$SpeedScore * (train$Ht / mean(RB$Ht)),
                        ifelse(
                          (train$Pos %in% "TE"),
                          train$SpeedScore * (train$Ht / mean(TE$Ht)),
                          ifelse(
                            (train$Pos %in% "WR"),
                            train$SpeedScore * (train$Ht / mean(WR$Ht)),
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
train$HA_SpeedScore <- as.numeric(train$HA_SpeedScore)

#Agility score: sum of Shuttle and Cone drill times, lower score is better
train$AgilityScore <- train$Cone + train$Shuttle

#Weight adjustedBurst Score: sum of Vertical and BroadJump, bigger score is better
train$BurstScore <- train$Vertical + train$BroadJump

#Final metric is athleticism score, which aggregates Forty time with agility score and burst score, and
#rewards players with a higher BMI
train$AthleticismScore <-
  ifelse(
    (train$Pos %in% "DB"),
    (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(DB$BMI)),
    ifelse(
      (train$Pos %in% "DE"),
      (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(DE$BMI)),
      ifelse(
        (train$Pos %in% "DT"),
        (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(DT$BMI)),
        ifelse(
          (train$Pos %in% "FB"),
          (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(FB$BMI)),
          ifelse(
            (train$Pos %in% "K"),
            (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(K$BMI)),
            ifelse(
              (train$Pos %in% "LB"),
              (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(LB$BMI)),
              ifelse(
                (train$Pos %in% "LS"),
                (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(LS$BMI)),
                ifelse(
                  (train$Pos %in% "OL"),
                  (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(OL$BMI)),
                  ifelse(
                    (train$Pos %in% "P"),
                    (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(P$BMI)),
                    ifelse(
                      (train$Pos %in% "QB"),
                      (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(QB$BMI)),
                      ifelse(
                        (train$Pos %in% "RB"),
                        (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(RB$BMI)),
                        ifelse(
                          (train$Pos %in% "TE"),
                          (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(TE$BMI)),
                          ifelse(
                            (train$Pos %in% "WR"),
                            (train$Forty + train$AgilityScore + train$BurstScore) * (train$BMI / mean(WR$BMI)),
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
train$AthleticismScore <- as.numeric(train$AthleticismScore)

library(ggplot2)

#Plot and see if it looks any better
ggplot(train, aes(x = Pick, y = SpeedScore)) + facet_wrap(~Pos) + geom_smooth() +
  geom_point() + labs(title = "SpeedScore vs. Pick by Position")

cor(train$SpeedScore, train$Pick, use = 'complete.obs') #-0.2678611, almost double what #BroadJump is!

ggplot(train, aes(x = Pick, y = HA_SpeedScore)) + facet_wrap(~Pos) + geom_smooth() +
  geom_point() + labs(title = "Height Adjusted Speed Score vs. Pick by Position")

cor(train$HA_SpeedScore, train$Pick, use = 'complete.obs') #-0.2721191, even better

ggplot(train, aes(x = Pick, y = AgilityScore)) + facet_wrap(~Pos) + geom_smooth() +
  geom_point() + labs(title = "AgilityScore vs. Pick by Position")

cor(train$AgilityScore, train$Pick, use = 'complete.obs') #0.06688, not very good

ggplot(train, aes(x = Pick, y = BurstScore)) + facet_wrap(~Pos) + geom_smooth() +
  geom_point() + labs(title = "BurstScore vs. Pick by Position")

cor(train$BurstScore, train$Pick, use = 'complete.obs') #-0.1374206, not bad

ggplot(train, aes(x = Pick, y = AthleticismScore)) + facet_wrap(~Pos) + geom_smooth() +
  geom_point() + labs(title = "AthleticismScore vs. Pick by Position")

cor(train$AthleticismScore, train$Pick, use = 'complete.obs') #-0.1361176, not bad

#As we can see, there appears to be a better predictive relationship between these feature
#engineered metrics and draft position than the standard ones

#As mentioned before, since data is highly non-linear and would be very difficult for a regression 
#model,we are going to use random forest models for each position. I chose the random forest model
#because of the fairly low number of observations and the fact that the data can be classified as
#not missing at random (because some or all variable could be missing from an observation if a 
#participant were to be injured for example)

#read in test data for predicting

test <- read.csv('test.csv')
test$X <- NULL
test$Pfr_ID <- NULL
test$AV <- NULL

#reassign positions for test, just like we did in train

test$Pos <- as.character(test$Pos)

test[which(test$Pos == "CB"), "Pos"] <- "DB"
test[which(test$Pos == "FS"), "Pos"] <- "DB"
test[which(test$Pos == "SS"), "Pos"] <- "DB"
test[which(test$Pos == "S"), "Pos"] <- "DB"
test[which(test$Pos == "EDGE"), "Pos"] <- "DE"
test[which(test$Pos == "G"), "Pos"] <- "OL"
test[which(test$Pos == "C"), "Pos"] <- "OL"
test[which(test$Pos == "OG"), "Pos"] <- "OL"
test[which(test$Pos == "OT"), "Pos"] <- "OL"
test[which(test$Pos == "ILB"), "Pos"] <- "LB"
test[which(test$Pos == "OLB"), "Pos"] <- "LB"
test[which(test$Pos == "NT"), "Pos"] <- "DT"

test$Team <- as.character(test$Team)
test[which(test$Team == ""), "Team"] <- "Undrafted"

#Add various metrics to test. Note: This would be easier to implement earlier, but since I've gotten
#this far, it would be more difficult to implement earlier

#BMI
test$BMI <- (test$Wt / test$Ht ** 2) * 703

#Speed score
test$SpeedScore <- (test$Wt * 200) / (test$Forty ** 4)  #Speed score rewards bigger players for
#being faster

#Height Adjusted Speed Score, rewards faster players who are taller, ifelse loop is going to be ugly
test$HA_SpeedScore <-
  ifelse(
    (test$Pos %in% "DB"),
    test$SpeedScore * (test$Ht / mean(DB$Ht)),
    ifelse(
      (test$Pos %in% "DE"),
      test$SpeedScore * (test$Ht / mean(DE$Ht)),
      ifelse(
        (test$Pos %in% "DT"),
        test$SpeedScore * (test$Ht / mean(DT$Ht)),
        ifelse(
          (test$Pos %in% "FB"),
          test$SpeedScore * (test$Ht / mean(FB$Ht)),
          ifelse(
            (test$Pos %in% "K"),
            test$SpeedScore * (test$Ht / mean(K$Ht)),
            ifelse(
              (test$Pos %in% "LB"),
              test$SpeedScore * (test$Ht / mean(LB$Ht)),
              ifelse(
                (test$Pos %in% "LS"),
                test$SpeedScore * (test$Ht / mean(LS$Ht)),
                ifelse(
                  (test$Pos %in% "OL"),
                  test$SpeedScore * (test$Ht / mean(OL$Ht)),
                  ifelse(
                    (test$Pos %in% "P"),
                    test$SpeedScore * (test$Ht / mean(P$Ht)),
                    ifelse(
                      (test$Pos %in% "QB"),
                      test$SpeedScore * (test$Ht / mean(QB$Ht)),
                      ifelse(
                        (test$Pos %in% "RB"),
                        test$SpeedScore * (test$Ht / mean(RB$Ht)),
                        ifelse(
                          (test$Pos %in% "TE"),
                          test$SpeedScore * (test$Ht / mean(TE$Ht)),
                          ifelse(
                            (test$Pos %in% "WR"),
                            test$SpeedScore * (test$Ht / mean(WR$Ht)),
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
test$HA_SpeedScore <- as.numeric(test$HA_SpeedScore)

#Agility score: sum of Shuttle and Cone drill times, lower score is better
test$AgilityScore <- test$Cone + test$Shuttle

#Weight adjustedBurst Score: sum of Vertical and BroadJump, bigger score is better
test$BurstScore <- test$Vertical + test$BroadJump

#Final metric is athleticism score, which aggregates Forty time with agility score and burst score, and
#rewards players with a higher BMI
test$AthleticismScore <-
  ifelse(
    (test$Pos %in% "DB"),
    (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(DB$BMI)),
    ifelse(
      (test$Pos %in% "DE"),
      (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(DE$BMI)),
      ifelse(
        (test$Pos %in% "DT"),
        (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(DT$BMI)),
        ifelse(
          (test$Pos %in% "FB"),
          (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(FB$BMI)),
          ifelse(
            (test$Pos %in% "K"),
            (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(K$BMI)),
            ifelse(
              (test$Pos %in% "LB"),
              (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(LB$BMI)),
              ifelse(
                (test$Pos %in% "LS"),
                (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(LS$BMI)),
                ifelse(
                  (test$Pos %in% "OL"),
                  (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(OL$BMI)),
                  ifelse(
                    (test$Pos %in% "P"),
                    (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(P$BMI)),
                    ifelse(
                      (test$Pos %in% "QB"),
                      (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(QB$BMI)),
                      ifelse(
                        (test$Pos %in% "RB"),
                        (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(RB$BMI)),
                        ifelse(
                          (test$Pos %in% "TE"),
                          (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(TE$BMI)),
                          ifelse(
                            (test$Pos %in% "WR"),
                            (test$Forty + test$AgilityScore + test$BurstScore) * (test$BMI / mean(WR$BMI)),
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
test$AthleticismScore <- as.numeric(test$AthleticismScore)
