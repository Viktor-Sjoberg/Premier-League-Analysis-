setwd("/Users/viktorsjoberg/Desktop/high-dimensional/Final project")
library(readr)
library(dplyr)
library(caret)


pl_18_19 <- read_csv("data/england-premier-league-2018-to-2019.csv")
pl_17_18 <- read_csv("data/england-premier-league-2017-to-2018.csv")

pl_17_18 <- select(pl_17_18, -c("LBH", "LBD", "LBA"))

teams_18_19 <- select(pl_18_19, c("HomeTeam", "AwayTeam"))
saveRDS(teams_18_19, "data/teams_18_19.rds")

pl_18_19_fin <- select(pl_18_19, -c(1,2,3,4,5,6,8,9,10,11))
pl_17_18_fin <- select(pl_17_18, -c(1,2,3,4,5,6,8,9,10,11))

#pl_18_19_fin$HomeTeam <- as.factor(pl_18_19_fin$HomeTeam)
#pl_18_19_fin$AwayTeam <- as.factor(pl_18_19_fin$AwayTeam)
pl_18_19_fin$FTR <- as.factor(pl_18_19_fin$FTR)
#pl_18_19_fin$Referee <- as.factor(pl_18_19_fin$Referee)
pl_18_19_fin$FTR <- as.numeric(factor(pl_18_19_fin$FTR, 
                                      levels = c("H", "D", "A"), 
                                      labels = c(1, 2, 3)))

#pl_17_18_fin$HomeTeam <- as.factor(pl_17_18_fin$HomeTeam)
#pl_17_18_fin$AwayTeam <- as.factor(pl_17_18_fin$AwayTeam)
pl_17_18_fin$FTR <- as.factor(pl_17_18_fin$FTR)
#pl_17_18_fin$Referee <- as.factor(pl_17_18_fin$Referee)
pl_17_18_fin$FTR <- as.numeric(factor(pl_17_18_fin$FTR, 
                                      levels = c("H", "D", "A"), 
                                      labels = c(1, 2, 3)))


saveRDS(pl_18_19_fin, "data/pl_18_19_fin.rds")
saveRDS(pl_17_18_fin, "data/pl_17_18_fin.rds")


set.seed(123)  # for reproducibility
trainingIndex <- createDataPartition(pl_17_18_fin$FTR, p = .8, list = FALSE)
trainingData <- pl_17_18_fin[trainingIndex, ]
testingData <- pl_17_18_fin[-trainingIndex, ]

x_test <- select(testingData, -c("FTR"))
x_train <- select(trainingData, -c("FTR"))
y_test <- select(testingData, c("FTR"))
y_train <- select(trainingData, c("FTR"))


x_test <- as.matrix(x_test)
x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
y_test <- as.matrix(y_test)

saveRDS(x_test, "data/x_test.rds")
saveRDS(x_train, "data/x_train.rds")
saveRDS(y_train, "data/y_train.rds")
saveRDS(y_test, "data/y_test.rds")

#### Results 
pl_18_19_fin_x <- select(pl_18_19_fin, -c("FTR"))
pl_18_19_fin_y <- select(pl_18_19_fin, c("FTR"))
pl_18_19_fin_x_s <- scale(pl_18_19_fin_x)

saveRDS(pl_18_19_fin_x_s,"data/pl_18_19_fin_x_s.rds")
saveRDS(pl_18_19_fin_y,"data/pl_18_19_fin_y.rds")
saveRDS(pl_18_19_fin_x, "data/pl_18_19_fin_x.rds")

