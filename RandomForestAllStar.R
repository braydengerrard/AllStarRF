library(data.table)
library(randomForest)
library(ROCR)

#All star data
AS <- read.csv("ASData.csv")
#Rookie data
R <- read.csv("RookieData.csv")
#Rookie advanced stats
Advt <- read.csv("RookieAdvData.csv")

R$AS <- as.factor(ifelse(R$Player %in% AS$Player,"AS","NotAS"))

R <- merge(R,Advt)
#League avg stats
Lgavg <- read.csv("Lgavg.csv")
Lg <- Lgavg[c(2,28)]
names(Lg)[2] <- "eFGAvg"

R <- merge(Lg,R)
R$efgnorm <- R$eFG.-R$eFGAvg #Normalize efg% to league avg

R[is.na(R)] <- 0

Rdown <- R[c(5,9,19,20,22:24,27,54,55,58,34,47,46)]

#Compute the optimal mtry
mtry <- tuneRF(Rdown[-12],Rdown$AS, ntreeTry=750,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
#Run Random Forest
All_Star_RFAdv <- randomForest(AS ~ efgnorm+PTS+STL+BLK+AST+ORB+DRB+MP+Age+FT.+USG.+DBPM+OBPM+TOV., mtry=best.m, ntree=750, data=R, importance=TRUE)


p1player <- R[c(4,34)]
pred <- predict(All_Star_RFAdv, type="prob")
predtab <- cbind(pred,p1player)
#Feature importantance plot
varImpPlot(All_Star_RFAdv)

#Save the importance plot
ggsave("AllStarRFImportance.png", type = "cairo-png", height = 5, width = 9.19)


Rookie21 <- read.csv("Rookie21.csv")
Rookie21$efgnorm <- Rookie21$eFG.-0.537
Rookie21ADV <- read.csv("Rookie21ADV.csv")
Rookie21t <- merge(Rookie21,Rookie21ADV)

#Get predictions for rookies from 20/21 seasonj
R21p <- predict(All_Star_RFAdv, Rookie21t, type = "prob")

R21predict <- cbind(R21p,Rookie21t$Player)

#ROCR
perf <- prediction(pred[,2], Rdown$AS)
auc <- performance(perf, "auc")

pred3 <- performance(perf, "tpr","fpr")
plot(pred3,main="ROC Curve",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

plot(All_Star_RFAdv, ylim = 0:1,main="Random Forest")
legend("topleft",legend=c("All Star","OOB","Non All Star"),lty=c(2,1,3),col=c("red","black","green"))

ggsave("Error.png", type = "cairo-png", height = 5, width = 9.19)
