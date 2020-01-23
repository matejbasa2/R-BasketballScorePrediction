setwd("C:/Users/Non5ens3/Desktop/UI/Seminarska")

CA <- function(observed, predicted)
{
  length(observed[observed == predicted]) / length(observed)
}
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

povp <- function(teamName, datum, parameter)
{
  where <- substring(parameter, 1, 1)
  if (where == "H") {
    nova <- regular[regular$DATE < datum,]
    home <- aggregate(nova[parameter], list(nova$HOME), mean)
    home[home$Group.1 == teamName,2]
  }
  else
  {
    nova <- regular[regular$DATE < datum,]
    home <- aggregate(nova[parameter], list(nova$AWAY), mean) 
    home[home$Group.1 == teamName,2]
  }
}

nova1 <- regular[regular$DATE < 20141029,]
home1 <- aggregate(nova1["H2PM"], list(nova1$HOME), mean) 
home1[home1$Group.1 == "LAL", 2]

regular <- read.table("regular.txt", header = T, sep = ",")
regular$HOME <- as.character(regular$HOME)
regular$AWAY <- as.character(regular$AWAY)
winners <- c()
loosers <- c()

for (i in 1:nrow(regular)) {
  if (regular$HPTS[i] > regular$APTS[i]) {
    winners[i] <- "H"
    loosers[i] <- "A"
  }
  else {
    winners[i] <- "A"
    loosers[i] <- "H"
  }
}

regular$WINNER <- winners
regular$LOOSER <- loosers
abs_Diff <- c()

for (i in 1:nrow(regular)) {
  abs_Diff[i] <- abs(regular$HPTS[i] - regular$APTS[i])
}

regular$ABS_DIFF <- abs_Diff

regular

allSeason <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(allSeason) <- c("DATE", "HOME", "HPTS", "H2PM", "H2PA", "H3PM", "H3PA", "AWAY", "APTS", "A2PM", "A2PA", "A3PM", "A3PA", "WIN", "ABS_DIFF")

for (i in 201:1230) {
  nov <- data.frame(regular$DATE[i],
                    regular$HOME[i],
                    povp(regular$HOME[i], regular$DATE[i], "HPTS"),
                    povp(regular$HOME[i], regular$DATE[i], "H2PM"),
                    povp(regular$HOME[i], regular$DATE[i], "H2PA"),
                    povp(regular$HOME[i], regular$DATE[i], "H3PM"),
                    povp(regular$HOME[i], regular$DATE[i], "H3PA"),
                    regular$AWAY[i],
                    povp(regular$AWAY[i], regular$DATE[i], "APTS"),
                    povp(regular$AWAY[i], regular$DATE[i], "A2PM"),
                    povp(regular$AWAY[i], regular$DATE[i], "A2PA"),
                    povp(regular$AWAY[i], regular$DATE[i], "A3PM"),
                    povp(regular$AWAY[i], regular$DATE[i], "A3PA"),
                    regular$WINNER[i],
                    regular$ABS_DIFF[i])
  colnames(nov) <- c("DATE", "HOME", "HPTS", "H2PM", "H2PA", "H3PM", "H3PA", "AWAY", "APTS", "A2PM", "A2PA", "A3PM", "A3PA", "WIN", "ABS_DIFF")
  allSeason <- rbind(allSeason, nov)
}
learn <- allSeason[1:824,]
test <- allSeason[825:1030,]

klasifikacija <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(klasifikacija) <- c("Metoda", "CA")
################################################KLASIFIKACIJA
majority <- names(which.max(table(learn$WIN)))
majority <- sum(test$WIN == majority) / length(test$WIN)

temp <- data.frame("majority", majority)
colnames(temp) <- c("Metoda", "CA")
klasifikacija <- rbind(klasifikacija, temp)
################################################VECINSKI
#install.packages("rpart")
library(rpart)
observed <- test$WIN
dt <- rpart(WIN ~ HPTS+APTS+H2PM+A2PM+H2PA+A2PA+H3PM+A3PM+H3PA+A3PA, data = learn, cp = 0.02, minsplit = 30)
plot(dt)
text(dt, pretty=0)

predicted <- predict(dt, test, type = "class")

temp <- data.frame("rpart", CA(observed, predicted))
colnames(temp) <- c("Metoda", "CA")
klasifikacija <- rbind(klasifikacija, temp)

#install.packages("randomForest")
library(randomForest)
observed <- test$WIN
rf <- randomForest(WIN ~ HPTS+APTS+H2PM+A2PM+H2PA+A2PA+H3PM+A3PM+H3PA+A3PA, data = learn)
predicted <- predict(rf, test, type = "class")

temp <- data.frame("randomForest", CA(observed, predicted))
colnames(temp) <- c("Metoda", "CA")
klasifikacija <- rbind(klasifikacija, temp)

#install.packages("e1071")
library(e1071)
nb <- naiveBayes(WIN ~ HPTS+APTS+H2PM+A2PM+H2PA+A2PA+H3PM+A3PM+H3PA+A3PA, data = learn)
predicted <- predict(nb, test, type="class")

temp <- data.frame("naiveBayes", CA(observed, predicted))
colnames(temp) <- c("Metoda", "CA")
klasifikacija <- rbind(klasifikacija, temp)

library(CORElearn)
nb <- CoreModel(WIN ~ HPTS+APTS+H2PM+A2PM+H2PA+A2PA+H3PM+A3PM+H3PA+A3PA, data = learn, model="bayes")
predicted <- predict(nb, test, type="class")

temp <- data.frame("CoreModel", CA(observed, predicted))
colnames(temp) <- c("Metoda", "CA")
klasifikacija <- rbind(klasifikacija, temp)

klasifikacija

####################################################REGRESIJA
allSeason <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(allSeason) <- c("DATE", "HOME", "HPTS", "H2PM", "H2PA", "H3PM", "H3PA", "AWAY", "APTS", "A2PM", "A2PA", "A3PM", "A3PA", "WIN", "ABS_DIFF")

for (i in 201:1230) {
  nov <- data.frame(regular$DATE[i],
                    regular$HOME[i],
                    povp(regular$HOME[i], regular$DATE[i], "HPTS"),
                    povp(regular$HOME[i], regular$DATE[i], "H2PM"),
                    povp(regular$HOME[i], regular$DATE[i], "H2PA"),
                    povp(regular$HOME[i], regular$DATE[i], "H3PM"),
                    povp(regular$HOME[i], regular$DATE[i], "H3PA"),
                    regular$AWAY[i],
                    povp(regular$HOME[i], regular$DATE[i], "APTS"),
                    povp(regular$AWAY[i], regular$DATE[i], "A2PM"),
                    povp(regular$AWAY[i], regular$DATE[i], "A2PA"),
                    povp(regular$AWAY[i], regular$DATE[i], "A3PM"),
                    povp(regular$AWAY[i], regular$DATE[i], "A2PA"),
                    regular$WINNER[i],
                    regular$ABS_DIFF[i])
  colnames(nov) <- c("DATE", "HOME", "HPTS", "H2PM", "H2PA", "H3PM", "H3PA", "AWAY", "APTS", "A2PM", "A2PA", "A3PM", "A3PA", "WIN", "ABS_DIFF")
  allSeason <- rbind(allSeason, nov)
}
learn <- allSeason[1:824,]
test <- allSeason[825:1030,]
learn$HOME <- as.factor(learn$HOME)
learn$AWAY <- as.factor(learn$AWAY)
test$HOME <- as.factor(test$HOME)
test$AWAY <- as.factor(test$AWAY)
learn$WIN <- as.factor(learn$WIN)
test$WIN <- as.factor(test$WIN)


regresija <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(regresija) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")

library(rpart)
observed <- test$ABS_DIFF
rt1 <- rpart(ABS_DIFF ~ ., data = learn)
predicted <- predict(rt1, test)

temp <- data.frame("rpart(neporezan) 1st", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

#plot(rt1)
#text(rt1, pretty = 0)

#Ga porezem
rt2 <- rpart(ABS_DIFF ~ ., data = learn)
rt2 <- prune(rt2, cp = 0.02)
predicted <- predict(rt2, test)

temp <- data.frame("rpart(porezan) 2nd", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

#plot(rt2)
#text(rt2, pretty = 0)

cm1 <- CoreModel(ABS_DIFF ~ ., data=learn, model="regTree", modelTypeReg = 1)
#plot(cm1, learn)
predicted <- predict(cm1, test)

temp <- data.frame("CoreModel 1st", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

cm2 <- CoreModel(ABS_DIFF ~ ., data=learn, model="regTree", modelTypeReg = 1, selectedPrunerReg = 2)
#plot(cm2, learn)
predicted <- predict(cm2, test)

temp <- data.frame("CoreModel 2nd", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

library(randomForest)
rf <- randomForest(ABS_DIFF ~ ., learn)
predicted <- predict(rf, test)

temp <- data.frame("randomForest", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

#install.packages("kknn")
library(kknn)
kknn <- kknn(ABS_DIFF ~ ., learn, test, k = 67)
predicted <- fitted(kknn)

temp <- data.frame("kknn", mae(observed, predicted), rmae(observed, predicted, mean(learn$ABS_DIFF)), mse(observed, predicted), rmse(observed, predicted, mean(learn$ABS_DIFF)))
colnames(temp) <- c("Metoda", "MAE", "RMAE", "MSE", "RMSE")
regresija <- rbind(regresija, temp)

regresija




#VIZUALIZACIJA 1
ekipe <- list(ucna$HOME)
ekipe <- unique(unlist(ekipe))
avg_HFTM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  HFTM <- 0
  j <- 0
  for (j in 1:length(ucna$HFTM))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HFTM <- HFTM + ucna$HFTM[j]
    } 
  }
  j <- 0
  for (j in 1:length(ucna$AFTM))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HFTM <- HFTM + ucna$AFTM[j]
    } 
  }
  avg_HFTM[k] <- HFTM/st_gameov
  k <- k+1
}
print(avg_HFTM)

avg_H2PM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe)){
  st_gameov <- 0
  H2PM <- 0
  for (j in 1:length(ucna$H2PM))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H2PM <- H2PM + ucna$H2PM[j]
    } 
  }
  for (j in 1:length(ucna$A2PM))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H2PM <- H2PM + ucna$A2PM[j]
    } 
  }
  avg_H2PM[k] <- (H2PM)*2/st_gameov
  
  k <- k+1
}
print(avg_H2PM)

avg_H3PM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  H3PM <- 0
  for (j in 1:length(ucna$H3PM))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H3PM <- H3PM + ucna$H3PM[j]
    } 
  }
  for (j in 1:length(ucna$A3PM))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H3PM <- H3PM + ucna$A3PM[j]
    } 
  }
  avg_H3PM[k] <- (H3PM*3)/st_gameov
  k <- k+1
}
print(avg_H3PM)


tocke <- data.frame(avg_HFTM, avg_H2PM, avg_H3PM)
par(mar=c(5, 4, 4, 2) + 0.1)
barplot(t(tocke), main="Ekipa/Povprecne Tocke", ylab = "AverageTocke", col=heat.colors(3), names.arg = ekipe, space=0.1, cex.axis=1, las=2, cex = 0.7, ylim = c(0,130))
legend(29, 150, names(tocke), cex=1, fill=heat.colors(3));

#VIZUALIZACIJA 2
ekipe <- list(ucna$HOME)
ekipe <- unique(unlist(ekipe))
avg_HFTA <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
avg_HFTM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  HFTA <- 0
  HFTM <- 0
  j <- 0
  for (j in 1:length(ucna$HFTA))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HFTA <- HFTA + ucna$HFTA[j]
      HFTM <- HFTM + ucna$HFTM[j]
    } 
  }
  j <- 0
  for (j in 1:length(ucna$HFTA))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HFTA <- HFTA + ucna$HFTA[j]
      HFTM <- HFTM + ucna$HFTM[j]
    } 
  }
  avg_HFTA[k] <- HFTA/st_gameov
  avg_HFTM[k] <- HFTM/st_gameov
  k <- k+1
}
print(avg_HFTA)
print(avg_HFTM)

avg_H2PA <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
avg_H2PM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  H2PA <- 0
  H2PM <- 0
  j <- 0
  for (j in 1:length(ucna$H2PA))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H2PA <- H2PA + ucna$H2PA[j]
      H2PM <- H2PM + ucna$H2PM[j]
    } 
  }
  j <- 0
  for (j in 1:length(ucna$H2PA))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H2PA <- H2PA + ucna$H2PA[j]
      H2PM <- H2PM + ucna$H2PM[j]
    } 
  }
  avg_H2PA[k] <- H2PA/st_gameov
  avg_H2PM[k] <- H2PM/st_gameov
  k <- k+1
}
print(avg_H2PA)
print(avg_H2PM)



avg_H3PA <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
avg_H3PM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  H3PA <- 0
  H3PM <- 0
  j <- 0
  for (j in 1:length(ucna$H3PA))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H3PA <- H3PA + ucna$H3PA[j]
      H3PM <- H3PM + ucna$H3PM[j]
    } 
  }
  j <- 0
  for (j in 1:length(ucna$H3PA))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      H3PA <- H3PA + ucna$H3PA[j]
      H3PM <- H3PM + ucna$H3PM[j]
    } 
  }
  avg_H3PA[k] <- H3PA/st_gameov
  avg_H3PM[k] <- H3PM/st_gameov
  k <- k+1
}
print(avg_H3PA)
print(avg_H3PM)

ASA = avg_HFTA + avg_H2PA + avg_H3PA
ASM = avg_HFTM + avg_H2PM + avg_H3PM

tocke <- data.frame((ASM/ASA), (ASA-ASM)/ASA)
par(mar=c(5, 4, 4, 2) + 0.1)
barplot(t(tocke), main="Povprecna procentna uspesnost meta neglede na to iz kje je",ylab = "AverageTocke", col=heat.colors(2), names.arg = ekipe, space=0.1, cex.axis=1, las=2, cex = 0.7)

legend(29, 150, names(tocke), cex=1, fill=heat.colors(2));
Average_Free_Throws_Made <- avg_HFTM/ASA
Average_Free_Throws_Missed <- (avg_HFTA-avg_HFTM)/ASA
Average_2_Pointers_Made <- avg_H2PM/ASA
Average_2_Pointers_Missed <- (avg_H2PA-avg_H2PM)/ASA
Average_3_Pointers_Made <-avg_H3PM/ASA
Average_3_Pointers_Missed <- (avg_H3PA-avg_H3PM)/ASA
tocke <- data.frame(Average_Free_Throws_Made,Average_2_Pointers_Made,Average_3_Pointers_Made, Average_Free_Throws_Missed,Average_2_Pointers_Missed,Average_3_Pointers_Missed)
par(mar=c(5, 4, 4, 2) + 0.1)
par(xpd=T, mar=par()$mar+c(5,0,0,0))
barplot(t(tocke), main="Povprecna procentna uspesnost meta neglede na to iz kje je", ylab = "AverageTocke", col=c('#ec0909', '#1bc81d', '#1e23c0', '#f95d5d', '#71f073', '#5d61e7'), names.arg = ekipe, space=0.2, cex.axis=1, las=2, cex = 0.7)
legend(-5, -0.17, names(tocke), cex=1, fill=c('#ec0909', '#1bc81d', '#1e23c0', '#f95d5d', '#71f073', '#5d61e7'));

#VIZUALIZACIJA 3

ekipe <- list(ucna$HOME)
ekipe <- unique(unlist(ekipe))
avg_HSTL <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  HSTL <- 0
  j <- 0
  for (j in 1:length(ucna$HSTL))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HSTL <- HSTL + ucna$HSTL[j]
    } 
  }
  j <- 0
  for (j in 1:length(ucna$ASTL))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HSTL <- HSTL + ucna$ASTL[j]
    } 
  }
  avg_HSTL[k] <- HSTL/st_gameov
  k <- k+1
}
print(avg_HSTL)

avg_HDRB <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe)){
  st_gameov <- 0
  HDRB <- 0
  for (j in 1:length(ucna$HDRB))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HDRB <- HDRB + ucna$HDRB[j]
    } 
  }
  for (j in 1:length(ucna$ADRB))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HDRB <- HDRB + ucna$ADRB[j]
    } 
  }
  avg_HDRB[k] <- HDRB/st_gameov
  
  k <- k+1
}
print(avg_HDRB)

avg_HORB <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  HORB <- 0
  for (j in 1:length(ucna$HORB))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HORB <- HORB + ucna$HORB[j]
    } 
  }
  for (j in 1:length(ucna$AORB))
  {
    if (ucna$AWAY[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HORB <- HORB + ucna$AORB[j]
    } 
  }
  avg_HORB[k] <- HORB/st_gameov
  k <- k+1
}
print(avg_HORB)

avg_HPTS <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
k <- 1
for (i in 1:length(ekipe))
{
  st_gameov <- 0
  HPTS <- 0
  j <- 0
  for (j in 1:length(ucna$HPTS))
  {
    if (ucna$HOME[j] == ekipe[i])
    {
      st_gameov <- st_gameov + 1
      HPTS <- HPTS + ucna$HPTS[j]
    } 
  }
  avg_HPTS[k] <- HPTS/st_gameov
  k <- k+1
}
print(avg_HPTS)
xx = ekipe
c(avg_HPTS/avg_HSTL)
yy = sort.int(c(avg_HPTS/avg_HSTL))
Average_Free_Throws_Made <- avg_HFTM/ASA
Average_Free_Throws_Missed <- (avg_HFTA-avg_HFTM)/ASA
Average_2_Pointers_Made <- avg_H2PM/ASA
Average_2_Pointers_Missed <- (avg_H2PA-avg_H2PM)/ASA
Average_3_Pointers_Made <-avg_H3PM/ASA
Average_3_Pointers_Missed <- (avg_H3PA-avg_H3PM)/ASA
ratio <- (avg_HFTM+avg_H2PM+avg_H3PM)/ASA
plot(x=ASA, y=ratio,main="Ratio zadetih kosev v odvisnosti od vseh metov", las=2)