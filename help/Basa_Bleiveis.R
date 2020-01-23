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

setwd("C:/Users/Non5ens3/Desktop/UI")
install.packages("e1071")
install.packages("CORElearn")
install.packages("rpart")
install.packages("randomForest")
install.packages("ggplot2")
library(e1071)
library(rpart)
library(ggplot2)
data(mpg, package="ggplot2")
library(randomForest)
library(CORElearn)


ucna$SEASON <- NULL
ucna$DATE <- NULL
ucna$HOME <- NULL
ucna$AWAY <- NULL
validacijska$SEASON <- NULL
validacijska$DATE <- NULL
validacijska$HOME <- NULL
validacijska$AWAY <- NULL
testna$SEASON <- NULL
testna$DATE <- NULL
testna$HOME <- NULL
testna$AWAY <- NULL
vsi_podatki <- read.table("regular.txt", header = T, sep=",")
ucna <- vsi_podatki[1:1230,]
validacijska <- vsi_podatki[1231:2460,]
testna <- vsi_podatki[2461:3690,]

povprecje_pred_tekmo <- function(ime_ekipe, datum, atribut){
    st_gameov <- 0
    a <- 0
    i <- 0
    for (i in 1:length(ucna$HFTM))
    {
      if (ucna$HOME[i] == ime_ekipe)
      {
        st_gameov <- st_gameov + 1
        a <- a + ucna$HFTM[i]
      } 
    }
    a <- 0
    j <- 0
    for (j in 1:length(ucna$AFTM))
    {
      if (ucna$AWAY[j] == ime_ekipe)
      {
        st_gameov <- st_gameov + 1
        b <- b + ucna$AFTM[j]
      } 
    }
    avg_a <- (a+b)/st_gameov
    return(avg_a)
}

abc <- povprecje_pred_tekmo('LAL', 10-10-2014, HPTS)

observed <- testna$HPTS
lm.model <- lm(HPTS ~ . , data = ucna)
predicted <- predict(lm.model, validacijska)

summary(observed)
mae(observed, predicted) #Mean Absolute Error

mse(observed, predicted)
rmse(observed, predicted, mean(ucna$HPTS))

rt.model <- rpart(HPTS ~ . , ucna)
predicted <- predict(rt.model, validacijska)
plot(rt.model);text(rt.model, pretty = 0)
mae(observed, predicted)
rmae(observed, predicted, mean(ucna$HPTS))

#REGRESIJSKO DREVO RPART

rt.model <- rpart(HPTS ~ . , data = ucna, minsplit=100)
plot(rt.model);text(rt.model, pretty = 0)

printcp(rt.model) #Napove za vsako vejo koliko se splaca (xerror)


rt.model2 <- prune(rt.model, cp = 0.02) #0.02, da bols kot karkoli drugega
plot(rt.model2);text(rt.model2, pretty = 0)
predicted <- predict(rt.model2, testna)
mae(observed, predicted)
rmae(observed, predicted, mean(ucna$HPTS))

#REGRESIJSKO DREVO CORElearn

rt.core <- CoreModel(HPTS ~ . , data = ucna, model = 'regTree', modelTypeReg=1)
plot(rt.model);text(rt.model, pretty = 0)
predicted <- predict(rt.core, validacijska)
mae(observed, predicted)
rmae(observed, predicted, mean(ucna$HPTS))

cm.nb <- CoreModel(HPTS ~ . , data = ucna, model = 'bayes')
plot(cm.nb);text(cm.nb, pretty = 0)
predicted <- predict(cm.nb, validacijska, type="class")
mae(observed, predicted)
rmae(observed, predicted, mean(ucna$HPTS))

#RANDOM FOREST (Nakljucni gozd)
observed <- testna$HPTS

rf.model <- randomForest(HPTS ~ H3PM+H2PM+HFTM, data = ucna)
plot(rf.model);text(rt.model, pretty = 0)
predicted <- predict(rf.model, testna, type = "class")
mae(observed, predicted)
rmae(observed, predicted, mean(ucna$HPTS))




#Naivni Bayes
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}
obsMat <- model.matrix(HPTS~-1, validacijska)
observed <- validacijska$HPTS

nb <- naiveBayes(HPTS ~ H3PM+H2PM+HFTM, data = ucna)
predicted <- predict(nb, validacijska, type="class")
CA(observed, predicted)
predMat <- predict(nb, validacijska, type = "raw")
brier.score(obsMat, predMat)

errorest(HPTS ~ H3PM+H2PM+HFTM, data=ucna, model = naiveBayes, predict = mypredict.generic)

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

