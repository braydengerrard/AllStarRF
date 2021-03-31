library(RANN)
library(data.table)
library(randomForest)
library(ROCR)


B <- read.csv("BBall.csv")

nn <- nn2(B$TS,B$TS.,k=20)

AS <- read.csv("AS.csv")
AS2 <- read.csv("AS2.csv")
AS3 <- read.csv("AS3.csv")

AS <- rbindlist(list(AS,AS2,AS3))

Rookie1 <- read.csv("Rookie1.csv")
Rookie2 <- read.csv("Rookie2.csv")
Rookie3 <- read.csv("Rookie3.csv")
Rookie4 <- read.csv("Rookie4.csv")
Rookie5 <- read.csv("Rookie5.csv")
Rookie6 <- read.csv("Rookie6.csv")
Rookie7 <- read.csv("Rookie7.csv")
Rookie8 <- read.csv("Rookie8.csv")
Rookie9 <- read.csv("Rookie9.csv")
Rookie10 <- read.csv("Rookie10.csv")
Rookie11 <- read.csv("Rookie11.csv")
Rookie12 <- read.csv("Rookie12.csv")
Rookie13 <- read.csv("Rookie13.csv")

Adv <- read.csv("RAdv1.csv")
Adv2 <- read.csv("RAdv2.csv")
Adv3 <- read.csv("RAdv3.csv")
Adv4 <- read.csv("RAdv4.csv")
Adv5 <- read.csv("RAdv5.csv")
Adv6 <- read.csv("RAdv6.csv")
Adv7 <- read.csv("RAdv7.csv")
Adv8 <- read.csv("RAdv8.csv")
Adv9 <- read.csv("RAdv9.csv")
Adv10 <- read.csv("RAdv10.csv")
Adv11 <- read.csv("RAdv11.csv")
Adv12 <- read.csv("RAdv12.csv")
Adv13 <- read.csv("RAdv13.csv")


R <- rbindlist(list(Rookie1,Rookie2,Rookie3,Rookie4,Rookie5,Rookie6,Rookie7,Rookie8,Rookie9,Rookie10,Rookie11,Rookie12,Rookie13))
R$AS <- as.factor(ifelse(R$Player %in% AS$Player,"AS","NotAS"))
R$ASn <- ifelse(R$Player %in% AS$Player,1,0)
Advt <- rbindlist(list(Adv,Adv2,Adv3,Adv4,Adv5,Adv6,Adv7,Adv8,Adv9,Adv10,Adv11,Adv12,Adv13))

R <- merge(R,Advt)

Lgavg <- read.csv("Lgavg.csv")
Lg <- Lgavg[c(2,28)]
names(Lg)[2] <- "eFGAvg"

R <- merge(Lg,R)
R$efgnorm <- R$eFG.-R$eFGAvg

#Set up data
data_set_size <- floor(nrow(R)/2)

# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(R), size = data_set_size)
# Assign the data to the correct sets
training <- R[indexes,]
validation1 <- R[-indexes,]

R[is.na(R)] <- 0

All_Star_RFAdv <- randomForest(AS ~ efgnorm+PTS+STL+BLK+AST+ORB+DRB+MP+Age+FT.+USG.+DBPM+OBPM+TOV., mtry=best.m, ntree=750, data=R, importance=TRUE)
All_Star_RF <- randomForest(AS ~ efgnorm+PTS+STL+BLK+AST+ORB+DRB+MP+Age+FT.,ntree=750, data=R, importance=TRUE)


p1player <- R[c(4,34)]
pred <- predict(All_Star_RFAdv, type="prob")
predtab <- cbind(pred,p1player)

varImpPlot(All_Star_RFAdv)

ggsave("AllStarRF.png", type = "cairo-png", height = 5, width = 9.19)


p1 <- predict(All_Star_RFAdv, R, type = "prob")
p <- predict(rf_classifier, R)



p11 <- cbind(p1,p1player)

Rookie21 <- read.csv("Rookie21.csv")
Rookie21$efgnorm <- Rookie21$eFG.-0.537
Rookie21ADV <- read.csv("Rookie21ADV.csv")
Rookie21t <- merge(Rookie21,Rookie21ADV)

R21p <- predict(All_Star_RFAdv, Rookie21t, type = "prob")
R21p <- predict(All_Star_RF, Rookie21t, type = "prob")

R21predict <- cbind(R21p,Rookie21t$Player)

predict(rf)

Rdown <- R[c(5,9,19,20,22:24,27,54,55,58,34,47,46)]

mtry <- tuneRF(Rdown[-12],Rdown$AS, ntreeTry=750,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)