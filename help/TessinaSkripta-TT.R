first
library(ggplot2)
library(mosaic)
library(psych)
library(vcd)
library(xlsx)
library(lmtest)
library(ggplot2)
library(car)
library(pwr)
library(scales)
library(ez)
library(reshape)
library(ggthemes)
library(pander)
library(cocor)
library(boot)
library(GPArotation)
library(WRS2)
library(missForest)
library(reshape2)

#UPORABNO
x1 <- subset(x, x$krrabiš==kr želiš)

#PORAZDELITVE (Normalna,kvadratna,hi^2)

n<-rnorm(1000000,100,15); hist(n,main=NULL); print(describe(n))
u<-runif(1000000,0,52); hist(u,main=NULL); print(describe(u))
c<-rchisq(1000000,1); hist(c,main=NULL); print(describe(c))

#vzorèna porazdelitev
n<-rnorm(vv,100,15); hist(n,main=NULL); print(describe(n))
u<-runif(vv,0,52); hist(u,main=NULL); print(describe(u))
c<-rchisq(vv,1); hist(c,main=NULL); print(describe(c))

#preizkušanje zakonov

#1
pon<-100000
vv=50
par(mfrow=c(2,1))

m<-rep(NA,pon)
for (i in 1:pon){
  m[i]<-mean(rnorm(vv,100,15))
}
hist(m,main=NULL)
qqPlot(m)
cat("Povpreèje M= ", mean(m), " SD(M)=",sd(m), " spl=", kurtosi(m), " asim= ", skew(m),  "\n")

#2

m<-rep(NA,pon)
for (i in 1:pon){
  m[i]<-mean(runif(vv,0,52))
}
hist(m,main=NULL)
cat("Povpreèje M= ", mean(m), " SD(M)=",sd(m), " spl=", kurtosi(m), " asim= ", skew(m),  "\n")

#3

m<-rep(NA,pon)
for (i in 1:pon){
  m[i]<-mean(rchisq(vv,1))
}
hist(m,main=NULL)
cat("Povpreèje M= ", mean(m), " SD(M)=",sd(m), " spl=", kurtosi(m), " asim= ", skew(m),  "\n")


#*************************************************
#POSTOPKI

#WILSONOV --> Raèunanje intervala zaupanja za dihotomno spr. 
#torej že imamo neke inf o naši populaciji
N1 <- 107
N<-310 
#(n1+n)

#podatki so iz vaj ESKS

mean(x$kariera)
table(x$kariera)
izp <- prop.test(107,310, conf.level = .99)

#Clopper - Pearsonov test --> tudi binomski test, konzervativen
#porazdelitev vzorènih deležev je asimetrièna, zato je tudi interval tak (spodaj omejen zgoraj ne) 
#--> èe gledamo same deleže pa more biti enako ker je omejeno na obeh straneh
izp <- binom.test(c(N1,N), conf.level = .99)
izp$conf.int - mean(x$kariera) #da vidimo, da je razlika med sp in zg mejo


#Pearsonov koeficient korelacije (tudi IZ)
#fisher, pretvorba..
cor(x$branje,x$ESKS) #dobimo samo korelacijo
cor.test(x$branje,x$ESKS, conf.level = .99)

#Èe želimo meriti veè korelacij hkrati

print(corr.test(x), short = F) # nam da tudi IZ, lahko dodamo tudi alpha=.01

#regresijski nagib
model <- lm(x$branje ~ x$ESKS)
#prva je y in druga x spr.
#intercept nam pove, koliko se poveèa y, èe poveèamo x za eno SD
#IZ dobimo s funkcijo confint

izb <- confint(lm(x$branje ~ x$ESKS), level =.99)

#*************************************************************

#Ocenjavanje parametrov II, evalvacija parametriènih IZ in zankanje

pon<-100000 #število vzorcev
n<-30 #število oseb v posameznem vzorcu

#1
rez_norm<-rep(NA,pon)
for (i in 1:pon){
  x<-rnorm(n,50,10)
  iz_norm<-c(mean(x)+(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1),mean(x)-(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1))
  rez_norm[i]<-(50>=iz_norm[2])&(50<=iz_norm[1])
}
print(table(rez_norm)*100/pon)

#2

rez_pravok<-rep(NA,pon)
for (i in 1:pon){
  x<-runif(n,0,100)
  iz_pravok<-c(mean(x)+(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1),mean(x)-(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1))
  rez_pravok[i]<-(50>=iz_pravok[2])&(50<=iz_pravok[1])
}
print(table(rez_pravok)*100/pon)

#3
rez_hi2<-rep(NA,pon)
for (i in 1:pon){
  x<-rchisq(n,2)
  iz_hi2<-c(mean(x)+(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1),mean(x)-(sd(x)/sqrt(length(x)))*qt(.975,length(x)-1))
  rez_hi2[i]<-(2>=iz_hi2[2])&(2<=iz_hi2[1])
}
print(table(rez_hi2)*100/pon)

#za konkretne podatke
#poda nam random izbrane podatke iz vzorca

x <- sample (podatki, length(x), replace = T)

#uporabno
describe(x)

#**********************************
  
  #zankanje!
#Vsaka stvar: M, SD, standardni odkloni.. vsaka ima svojo funckijo, ki jo daš v boot

#povpreèni absolutni odklon od aritmetiène sredine

PAO <- function(x){
  pao <- mean(abs(x-mean(x)))
  return(pao)}
#uporabimo PAO(x) da dobimo to (to je spisana funkcija)

# boot

#najprej rabimo še en PAOz
PAOz <- function(x,i){
  pao<-mean(abs(x[i]-mean(x[i])))
  return(pao)
}
#Ko damo v boot PAO, damo èisto samo funkcijo in nobenim podatkov! ,PAO, NE pa PAO(x)!!!!!!!!!!!
boot_rez <- boot(podatki, PAOz, št. ponovitev)

#boot.ci

iz<- boot.ci(boot_rez, conf=.95, type="bca") #type = "bca"

#Funkcija za ARITMETIÈNE SREDINE
mz <-function(x,i){
  m <- mean(x[i])
  return(m)
}

boot_rez <-boot(x$branje, mz, 10000)

#funkcija za STANDARDNO DEVIACIJO

sz <- function(x,i){
  s <- sd(x[i])
  return(s) }

boot_rez <- boot(X$branje, sz, 10000)
iz <- boot.ci(boot_rez, conf = .95,type =c("bca", "perc"))

#funkcija za korelacijo

fr <- function(x,i) {
  r <- cor(x[i,1], x[i,2])
  return(r)
}

#funkcija za Korelacijsko regresijo
fb <- function(x,i){
  b <- lm(x[x,i]~x[i,2])$coefficients
  return(b) }
  
bb <- boot(x,fb,2000)
plot(bb, index=1)

#KONEC ZANKASTIH FUNKCIJ
#*********************************************************************

#HIPOTEZE --> Preverjanje veljavnosti
#Preverjamo ali lahko dobimo drugaèno M zgolj zaradi napake vzorèenja
m <- mean(x)
sn <- sd(x)/sqrt(length(x)) #to je SE
t <- (m-prièakovana m)/sn #dobimo dejanski t(kot z)

#zdaj ugotavljamo ali je t vrednost stat. znaèilna ali ne
#imamo dve poti raèunanja

#1 --> raèunamo, kakšen je kritièen t za doloèen IZ
qt(p=.025,df=65) #spodnji t
qt(p=.025,df=65,lower.tail = F) #zgornji t

#IZ -t(c)= -2,00 in t(c)= 2,00

#Imamo tudi obratno funkcijo --> Zraèuna % pod toèko

pt(t,65)

#T TESTI!!!!!!!!!
#pomembno: Normalna porazdelitev, homogenost varianc, razmerja var1/var2 <3
#Dvostranski t test za 1 vzorec

t.test(x$Ocena, mu=0,alternative = c("two.sided", "less", "greater"), conf.level = 0.95)

#"less" pomeni: ???M < c; vpišemo lahko tudi samo "t", "l" oz. "g". Pazi na mu!


#T test za odvisna vzorca in za nedovisna (samo spustimo paired)

ttest <- t.test(x1, x2, mu=0, alternative = "two.sided", "less", "greater", conf.level = .95, paired = T)
#Poroèanje: Ms=43.65, Md=42.03,t(149) = 2,80, p= 0,006

#Lahko pogledamo znotraj t testa: p.value, statistic, conf.int..


#Velikost uèinka, Cohenov d (pove nam ali je razlika, ne pa dejansko kolišna je)
# 0,2 - majhen, 0,5 - srednji, 0,8 - velik UÈINEK

#t test za odvisne, lahko raèunamo tudi na roke:
d <- x1 - x2
mean(d)
dz <- mean(d) / sd(d) # to je cohenov d

#**************************************************************************************
#NEPARAMETRIÈNI TESTI ZA ODVISNE SPREMENLJIVKE
#Najprej naredimo box plot

#Yuenov robustni preizkus
#razlika med winsoriziranima M, uporabimo pri odstopanju od normalnosti ali prisotnosti osamelcev

yuend(x1,x2)

#Wilcoxonov preizkus enakovrednih parov --> razlike v mediani

wilcox.test(x1,x2,alternative = c("two.sided", "less", "greater"), paired = T,conf.int = FALSE, conf.level = 0.95)

#************************************************************************
#NEODVISNE SPREMENLJIVKE!!!!!!!!!!!!!!!!


#Domneve o razliki med centralnima tendencama – neodvisna vzorca

Boxplot(x~y)
plot(density(y[x==1])) #??? gostota verjetnosti neodvisne spremenjlivke
lines(density(y[x==1]), lty=2)

#T test za neodvisne
#primerjava aritmetiènih sredin, 2 populacij
t.test(x1, x2, alternative=c("two.sided", "less", "greater"),var.equal = FALSE, conf.level = 0.95)
#pri enakosti varianc damo False èe nismo preprièani, ne izgubimo veliko moèi

#èe želimo sami zraèunati razliko med AS
ttest$estimate[2]-ttest$estimate[1]
#ali pa:
mean(x$empatija[x$ženska==1])-mean(x$empatija[x$ženska==0])

#Cohenov d
s <- sqrt((82*var(x$empatija[x$ženska==0]) + 105*var(x$empatija[x$ženska==1]))/ 185)
s
[1] 3.880823
#Cohenov d, ??? velikost uèinka

d <- raz/s
 d
mean in group 1 
0.9099511 
 cor(x$empatija, x$ženska)**2
0.1724325

#yuen-Welchov robustni preizkus
#Testiramo razliko med prirezanima (trimmed) aritmetiènima sredinama, torej H0: ???t1 = ???t2. Uporabimo pri nenormalnih porazdelitvah in/ali prisotnosti osamelcev. 
yuen(spremenljivka~skupina)

#z zankanjem
yuenbt(spremenljivka~skupina,nboot = 599)

#Mann-Whitneyec U preizkus/ Wilcoxonov za neodvisna vzorca
#vsaj ordinalna spr.
wilcox.test(spremenljivka~skupina, alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95)
#Èe vzorec ni prevelik in nimamo vezanih rangov, nastavimo exact=T. ??? upošteva, da sta spremenljivki neodvisni? 

#***************************************************************************************

#Preizkušanje domnev: domneve o korelacijah, variancah in obliki porazdelitve

#Oblika porazdelitve
shapiro.test(x$neki)
#èe je p stat. znaèilen, pomeni, da porazdelitev ni normalna

# s histogramom pogledamo porazdelitev, splošèenost in asimetriènost

#Koeficient korelacije
cor.test(x1,x2)

#plot, da preverimo razpršenost

#Pearsonov korelacija (h0 in IZ)
rtest <- cor.test(x,y,alternative =c("two.sided", "less", "greater"), conf.level = 0.95)

#Kendall T test
cor.test(x1,x2,method = "k")

rtest <- cor.test(x,y,alternative = c("two.sided", "less", "greater"), method=c("kendall”,"spearman")) 
#IZ doloèimo z zankanjem

#Statistièna znaèilnost razlike med dvema koeficientoma korelacije 
#neodvisna vzorca
cocor.indep.groups(r1, r2, n1, n2, alternative = "two.sided",test = "all",alpha = 0.05, conf.level = 0.95, null.value = 0)
                                                                                
#odvisna vzorca

cocor.dep.groups.overlap(r.xy, r.xz, r.yz, n, alternative = “two.sided", test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0)

#****************************

#Domneva o enakosti varianc v neovisnih vzorcih

# za opisni pregled imamo: describeBy, tapply, boxplot

# F preizkus za neodvisna vzorca

var.test(spremenljivka~skupina, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)


#Levenov test za neodvisne vzorce
leveneTest(spremenljivka~skupina)

#Flignr-Killeenov preizkus za neodvisne vzorce
#uporablja range - neparametrièen?

fligner.test(spremenljivka~skupina)

## Normalnost porazdelitve

#Shapiro-wilkinsov test
shapiro.test(x)


#********************************************************************************************************************************
#ANOVA

#Enosmerna ANOVA --> Intervalna odvisna spr, faktorz diskretnimi vrednostmi
#Pogoji: nakljuèno vzorèenje, homogenost varianc, normalna porazdelitev

#normalnost: Kruskal.test

#Èe je faktor kodiran s številkami, ga moramo nujno opredeliti kot faktor!


f <- as.factor(f) ali X$f <- as.factor(X$f)
plot(x$od~x$izobrazba,x)

#izvedemo analizo

anovarez <- aov(x$od~x$izobrazba,x)
#èe želimo klasièno tabelo:
anovasum <- summary(anovarez)

#posamezni podatki:
anovasum[[1]]$Df
anovasum[[1]]$F[1] 
anovasum[[1]]$Pr[1] 


#Velikost uèinka (eta) -->Velikost uèinka tipièno opredelimo kot % variance OS, pojasnjen z NS
eta2 <- anovasum[[1]]$`Sum Sq`[1] / (anovasum[[1]]$`Sum Sq`[1] + anovasum[[1]]$`Sum Sq`[2])

#OMEGA IDK
Omegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  msError    <- sumAov[residRow,3]
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  ssTotal    <- rep(sum(sumAov[1:residRow, 2]), residRow-1)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- rownames(sumAov)[1:{residRow-1}]
  Omegas
}

partialOmegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  dfError    <- sumAov[residRow,1]
  msError    <- sumAov[residRow,3]
  nTotal     <- nrow(model.frame(aovMod))
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
  partOmegas }
  

#Alternativni testi

#kršena enakost varianc - Welchov preizkus
oneway.test(x$od~x$izobrazba,x)
#damo argument var.equal=t indobimo obièajni F test

#odstopanje od normalnosti, robustni preizkus
#primerjava prirezanih aritmetiènih sredin
t1waybt(x$od~x$izobrazba,x,nboot = 1000) #nboot naj bo velik

#primerjava median
med1way(x$od~x$izobrazba,x)

#ordinalna spremenljivka ali odstopanje od normalnosti porazdelitve
kruskal.test(x$od~x$izobrazba,x)


#*************
#POST HOC preizkusi

TukeyHSD(anovarez) # argument je stvar, ki jo dobimo s funkcijo aov
#Lwr in upr sta IZ

#Bolforonijev preizkus--> nizka vrednost alfa napake, ampak tudi nižja moè, konzervativen
#Nimam ga lol

#èe pa smo uporabili robustni test
lincon(x$od~x$izobrazba,x)
#bodi pozorna na IZ ne toliko na p

#KONTRASTI
#vsota koeficientov=0
contrasts(x$izobrazba) <- cbind(c(-1,-1,2),c(1,-1,0))
summary.lm(aov(x$od ~ x$izobrazba, x))

#************************************************************************************

#NOMINALNE SPREMENLJIVKE

#Hipoteze o deležu dihotomne spremenljivke

binom.test(n1,n,p=0.5, alternative = c("t","l","g"), conf.level = 0.95)
#x = število oseb z vrednostjo, ki nas zanima, n = število vseh oseb, p = predpostavljeni delež (privzeto 0,5).
#P je ekstaksten, IZ pa je konzervativen


#Hipoteze o deležih spremenljivke z veè deleži
#Pearsonov hi^2 --> raèunamo na roke, da preverimo rezultat
#fe: opazovane (»empiriène«, observed) frekvence
#ft: prièakovane (»teoretiène«, expected) frekvence

#postopek: naredimo table iz spr. ki nas zanima, potem pa table delimo z N, da vidimo kakšna so razmerje
ft <- table(x$slog)
ft/length(x$slog)
#zdaj moramo ugotoviti ali se skupine med seboj stat. znaèilno razlikujejo
#zraèunamo kako se normalno porazdeljujejo (naj bi se)
length(x$slog)/k
#Prièakovane F osnujemo na teoriji te razporeditve, 1/3 na 95 oseb
#potem pa raèunamo po formuli, medtem da morajo biti pridobljene številke cele, teoretiène pa ne
#npr ((21-31.7)^2/31.7)+((57-31.7)^2/31.7)+((17-31.7)^2/31.7)

#lahko pa uporabimo test
chisq.test(x, p=c(1/k)simulate.p.value = FALSE, B = 2000) #èe nimamo H0 enakih deležev potem naredimo p=c(0.2,0.6,0.2)
#lahko pa damo samo ft kot x
hirez <- chisq.test(ft)
#navajanje: H^2(2)=30,65;p<',001

#lahko pogledamo:
hirez$expected
hirez$observed
hirez$residuals
hirez$observed-hirez$expected #dobimo dejanske vrednosti odstopanja, surove razlike

# POVEZANOST NOMINALNIH SPREMENLJIVK
#H0 lahko interpretiramo tudi v smislu enakosti profilov deležev X pri vseh vrednostih Y.
table(x,y) #da vidimo, ali so že takoj opazne kakšne razlike
hk <- chisq.test(x,y, correct=T) #lahko navedemo tudi kontingenèno tabelo table(x,y)
hk
hk$residuals #nam pove kakšne je odstopanje od prièakovanih in dobljenih frekvenc


#Razlika med deležema pri odvisnih vzorcih (imamo 2 dihotomni spr)
#H0; delež pozitivnih vrednosti je enak pri x in y
#Imamo 2 nalogi, dihotomno toèkovani 1 prav, 0 narobe in zanima nas ali sta nalogi enako težki, ali je delež pravilnih odgovor enak pri obeh nalogah
#uporabimo Mcnemarjev preizkus
hi^2=(B-C)^2/B+C
mcnemar.test(x,y,correct=T)
mcnemar.test(table(x,y), correct = T)

#èe ni stat znaèilno, to pomeni, da so razlike med pravilnimi rezultati zgolj posledice sluèajnih razlig.. ni ena naloga težja od druge

#************************************************************************

#ANALIZA MOÈI STAT. PREIZKUSOV
#Manjša verjetnost alfe = veèja verjetno bete
#Moè testa --> 1-beta = verjetnost, da pravilno zavrnemo napaèno H0 
#Moè je veèja kadar: 1. velik N, 2. IZ je veèji, 3: Velikost uèinka veèja + izbira testa
#naj bi bila vsaj 80%

#V funkcijah pwr....test lahko definiramo velikost vzorca (n), velikost uèinka, raven tveganja (sig.level) in zaželeno moè (power). Parameter, ki ga želimo izraèunati, izpustimo ali definiramo kot NULL. Èe želimo doloèiti optimalno raven tveganja, obvezno zapišemo sig.level=NULL, ker ima ta parameter privzeto vrednost 0,05.

#T test za en vzorec, odvisna ali enako velika neodvisna vzorca
mt <- pwr.t.test(n=NULL,d=NULL,sig.level = NULL,power = NULL, type = c("two.sample","one.sample","paired"), alternative = c("two.sided","g","l"))
plot(mt) #da vidimo krivuljo moèi
#power=.9 (i guess 10% beta napake)
#Dopušèamo si verjetnost beta napake najveè 10%

#cohenov d:
cohen.ES(test = "t", size = "s") #also size M, L

#t preizkus za neodvisna vzorca
m2t <- pwr.t2n.test(n1=NULL,n2=NULL,d=NULL,sig.level = NULL,power = NULL, alternative = c("t","l","g"))

#preizkus stat. znaèilnosti koeficienta korelacije
pwr.r.test(n = NULL, r = NULL, sig.level = 0.05, power = NULL, alternative = c("two.sided", "less","greater"))
#vemo koliko ljudi lahko zberemo in kakšno moè imamo, zanima pa nas kako visoka mora biti korelacija, da jo bomo z doloèeno verjetnostjo dokazali

#ENOSMERNA ANOVA
pwr.anova.test(k=NULL, n=NULL, f=NULL, sig.level = NULL, power = NULL)

#k= št. skupin, f= velikost uèinka


#I MADE IT!











#Multivariatne metode


#****************************** PROFESORJEVA SKRIPTA***********************************************








options(digits=3, scipen=T) #prikaz na manj dec. mest; "penaliziramo" "znanstveni" prikaz vrednosti
setwd("vpišite svojo mapo")
library(car)
library(psych)

# Hitro kopiranje v Excel
kex<-function(X){
  write.table(X,"clipboard",sep="\t",dec=",",row.names=T)
}

#Frekvenèna tabela za veè spremenljivk, s kumulativnimi frek. in %
#manjkajoèe vrednosti so izpušèene
frektabela<-function(X){
  X<-as.data.frame(X)
  frektabela<-vector("list",dim(X)[2])
  for (j in 1:dim(X)[2]){
    x<-X[,j]
    ime<-names(X)[j]
    f<-table(x)
    i<-names(f)
    f<-as.vector(f)
    fc<-cumsum(f)
    p<-100*f/sum(f)
    cp<-cumsum(p)
    tabela<-data.frame(i,f,fc,p,cp)
    names(tabela)<-c(ime,"f","cf","%","cum%")
    cat("\n")
    print(tabela)
    cat("\n")
    if(max(fc)<dim(X)[1]){
      cat("Spremenljivka ",ime," ima ",dim(X)[1]-max(fc)," manjkajoèih vrednosti.")} 
    cat("\n")
    frektabela[[j]]<-tabela
  }
  names(frektabela)<-names(X)
  return(frektabela)
}

# Histogram s krivuljo gostote in normalno krivuljo
histkriv <- function(x,ime="Vrednost"){
  hist(x,breaks="FD",freq=F,main=NULL,xlab=ime,ylab="Verj. gostota",ylim=c(0,max(c(max(dnorm(min(x):max(x),mean=mean(x),sd=sd(x)),max(hist(x,breaks="FD",plot=F)$density),max(density(x)$y))))))
  lines(x=min(x):max(x),y=dnorm(min(x):max(x),mean=mean(x),sd=sd(x)),lty=2)
  lines(density(x))  
}


# Q-Q grafikon in zaboj z roèaji Mahalanobisovih razdalj
# Zahteva paket car!
mahalqq <-function(X){
  require(car)
  D2 <- mahalanobis(X, colMeans(X,na.rm=T), var(X,na.rm=T))
  n<-dim(X)[1]
  p<-dim(X)[2]
  qqplot(qchisq(ppoints(n), df = p), D2,xlab="Kvantili hi**2",ylab="Kvadrirana Mahal. razd.")
  abline(0, 1, col = 'gray')
  Boxplot(D2)
}


#V tabeli izpiše kljuène rezultate reg.analize
regtabela<-function(lmr){
  y<-lmr$model[,1]
  X<-lmr$model[,-1]
  p<-dim(X)[2]
  tab <- data.frame(summary(lmr)$coefficients[,1:4],confint(lmr))
  
  zy<-scale(lmr$model)[,1]
  zx<-scale(lmr$model)[,-1]
  beta <- coef(lm(zy~zx))
  
  r2 <- summary(lmr)$r.squared
  kspr<- rep(NA,p+1)
  toler <- rep(NA,p+1)
  for (i in 1:p){
    lmi <- lm(y~as.matrix(X[,-i]))
    kspr[i+1] <- r2 - summary(lmi)$r.squared
    lmi <- lm(X[,i]~as.matrix(X[,-i]))
    toler[i+1] <- 1 - summary(lmi)$r.squared
  }
  tab <- cbind(tab,beta,kspr,c(NA,vif(lmr)),toler)
  names(tab) <- c("b","SE","t","p","IZ95%sm","IZ95%zm","beta","kspr","VIF","Tol.")  
  print(round(tab,digits=3))
  tab<-cbind(row.names(tab),tab)
  names(tab)[1]<-" "
  return(tab)
}

# Ocene regr.koeficientov z zankanjem
# (deloma preplonkano od Foxa & Weisberga, 2011)
regzank <- function(lmr){
  betahat.boot <- bootCase(lmr, B=999)
  usualEsts <- summary(lmr)$coef[ , 1:2]
  bootSD <- apply(betahat.boot, 2, sd) 
  bootEst <- colMeans(betahat.boot)
  bootBias <- (bootEst - usualEsts[ , 1])/usualEsts[ , 2]
  bootCI <- apply(betahat.boot, 2, function(x) quantile(x, c(.025,.975)))
  bsrez <- data.frame(usualEsts, bootBias, bootSD, t(bootCI))
  names(bsrez) <- c("b","SE","pristr.","SE_bs","95%IZsm","95%IZzm")
  print(bsrez, digits=3)
  bsrez<-cbind(row.names(bsrez),bsrez)
  names(bsrez)[1]<-" "
  return(bsrez)
}


#Preizkus prileganja modela logistiène regresije po Hosmerju in Lemeshowu
# x = vektor izidov(1/0), y = vektor napovedanih verjetnosti, g = število intervalov
#Avtor kode: Peter Solymos
hoslem<-function (x, y, g = 10) 
{
  DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), 
                 sep = ", ")
  METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
  yhat <- y
  y <- x
  qq <- unique(quantile(yhat, probs = seq(0, 1, 1/g)))
  cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
  observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ 
                      cutyhat)
  chisq <- sum((observed - expected)^2/expected)
  cat("Prièakovane in dejanske frekvence \n")
  print(cbind(expected[,2],observed[,2]))
  PVAL = 1 - pchisq(chisq, g - 2)
  PARAMETER <- g - 2
  names(chisq) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = chisq, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed, 
                 expected = expected), class = "htest")
}

#Klasifikacijska tabela za logistièno regresijo (ali drug glm model)
klastabela<-function(model){
  usp0<-100*max(table(model$y))/sum(table(model$y))
  napovedane <- round(model$fitted.values)
  dejanske <- model$y
  cat("Klasifikacijska tabela: \n")
  kt <- table(napovedane, dejanske)
  print(kt)
  usp1 <- 100*sum(diag(kt))/sum(kt)
  cat(" \n")
  cat("Klasifikacijska tabela v %: \n")
  print(prop.table(kt))
  cat(" \n")
  cat("Odstotek uspešnih napovedi s praznim modelom: ", usp0, "\n")
  cat("Odstotek uspešnih napovedi z izbranim modelom: ", usp1, "\n")
  kt
}






#******************KONEC PROFESORJEVE SKRIPTE***********************************
#INDEKSIRANJE: [vrstice, stolpec] 
#Prazno mesto pomeni vse [, st] – pomeni vse vrstice 
#Os [, 2:10] – stolpci od dva do osem 
#Os [, -1] – vsi stolpci razen prvega 

x2 <- na.omit(x2) #brisanje NA

#Za pregled podatkov lahko uporabimo;
str(x) #izvemo vrste spremenljivk

#èe bi radi videli pregled spremenljivke oz njenih levelov in frekvenc (frekvenèni prikaz):
table(x$moški) #da vidimo ali se pojavljajo nemogoèe vrednosti

#èe želimo pretvoriti v deleže;
prop.table(table(x$moški)) #*100 za odstotke
apply(x[,-1],2, table) #za veè spremenljivk hkrati

#kumulativne frekvence in odstotki za celo tabelo
frektabela(x) #moraš skripto aktivirat

summary(x[,-1]) #[] zato, da smo izloèili št osebe
#izvemo št manjkajoèih vrednosti
describe(x) #pove tudi splošèenost, asimetriènost.. veè info
describeBy (x, group=x$moški) #ali describeBy(x[,2:5](ali [,c(1,2,3,5])), group=x$moški)
tapply(x$gfaktor, x$moški, mean, na.rm = TRUE) #loèi glede na spremenljivko


#GRAFIÈNI PRIKAZI**************************************

h <- hist(x$prostorski)

#histogram s krivuljo gostote
hist(x$prostorski,freq=F) #y os je zdaj gostota
lines(density(x$prostorski))  #doda èrte na obstojeèo sliko
#èe hoèemo še normalno krivuljo;
> histkriv(x$prostorski)
#za prikaz veèih histogramov hkrati;
par(mfrow=c(2,3)) 
apply(x[,3:8],2,histkriv) #1. arg na katerih podatkih, 2. kakšen prikaz želimo, 3. katero funkcijo

#zaboj z roèaji
Boxplot(x$prostorski)

#razsevni diagram****
plot(x,y)
plot(x~y)
scatterplot()
sp(x$besedni~x$prostorski) #pokaže regresijsko premico
#èe se rdeèa in zelena èrta ne ujemata, potem je bolje, da uporabimo analizo za nelinearno povezanost
scatterplotMatrix(x) #ali spm, prikaz gostote in sp za vse spremenljivke

#matrika razsevnih diagramov
pairs(x)
r <- cor(x[,4:8]) #dobimo graf z vsemi želenimi korelaciji, s cor.plot ga spremenimo v barvni graf
cor.plot(cor(x), numbers=T, cex=1.5)
identify(x,y) #klikneš na graf in pove toèko (mogoèe)



#ISKANJE OSAMELCEV z Mahalanobisovo razdaljo (multi. posplošitev Z vrednosti)***********************************
#Multivariatni osamelci – osebe z nenavadno kombinacijo vrednosti. 
#Univariatne osamelce najdemo najlaže z zabojom z roèaji. 
#Bivariatne osamelce z razsevnim diagramom. 
mahalanobis(x[,4:8]) #ne vem kako dela funkcija
mahalqq(x[,4:8])

#MANJKAJOÈE VREDNOSTI
#lahko jih izbrišemo ali ocenimo

rowSums(is.na(x)) #izvemo v katerih vrstah so NA
is.na(x) #pokaže prov na katerem mestu je napaka; piše TRUE
na <- rowSums(is.na(x))
table(na) #koliko oseb nima NA, koliko 1, koliko 2..
sum(na) #št. na


#Normalnost porazdelitve*********************************
#rabimo library("car")

qqPlot(x$gfaktor)
qqnorm(x$gfaktor)

densityBy(x[,5:8]) #"violinski" grafi --> vodoravno gledano vidimo porazdelitev
#densityBy(x, grp=a)
densityBy(x[,5:6], grp = x$starost)

#stat. preizkus hipoteze multivariatne normalnosti
mardia(x$besedni)
shapiro.test(x)


#Lestvièenje*************************
#da standardiziramo uporabimo scale;
z <- data.frame(scale(x)) #centriranje: M=0: c <- data.frame(scale(x,scale=F))

#*******************MULTIPLA REGRESIJA***************************

#1. pregledamo podatke (poglej na zaèetek MVM)
summary,describe..,

#2. porazdelitev odvisne spremenljive
#vseeno je kakšna je porazdelitev napovednikov, si pa želimo normalne --> veèja možnost za N.P. napak
#odvisna spremenljivka mora bizi blizu N.P.
shapiro.test(x$DU)

#3. koeficienti korelacije
cor(x)
#najbolj pozorni smo na korelacije z odvisno spremenljivko
#ali je statistièno znaèilno:
corr.test(x)

#osnovna regresijska analiza
#izdelamo regresijski objekt z lm (linear model):
lmr <- lm(DU~TN+PRI+NAT+VZT, data=x)
lmr #izpis koeficientov in parametrov modela
#Intercept = konstanta (to prištejemo vsem ljudem, ne glede na rezultate na napovednikih) = 11,7 ??? pomeni napovedano vrednosti za osebo, ki ima vrednosti vseh napovednikov enako niè. 
#DUi' = 11,7 + 0,09*b1 + 0,15*b2 + 0,06*b3 + 0,17*b4 = NAPOVEDANA USPEŠNOST 
#primer1: imaš 2 osebi, ki imata pri osebnostnih lestvicah vse enako, pri testu nizov pa se razlikujeta za 1 toèko.  Ti dve osebi se bosta na prièakovani (ne nujno dejanski) uspešnosti razlikovali za 0,09 (tista z višjo TN bo imela za 0,09 napovedane vrednosti)


s <- summary(lmr)
#èe je stand. error veèji od koeficienta, je reg. nagib zelo nenatanèno doloèen
#èe ni stat. znaèilen nagib (Pr(t)), potem moramo popraviti model, saj to pomeni, da ne moremo zavrniti nièelne hipoteze
#residual Stand. error --> za koliko SD v povpreèju so rezultati oddaljeni od H0 (nièelni model) - visok SD, slabo
#multiple R^2 -> delež pojasnjene variance, adjusted R pa je realistièna pojasnjena varianca glede na populacijo
#veèji F pomeni manjšo natanènost -> pojasnjena var/nepojasnjena var

#obsežnejši povzetek modela
rt <- regtabela(lmr) #v profesorjevi skripti
#dobimo:
#•	b: regresijski koeficienti;
#•	SE: njihove standardne napake;
#•	t: t vrednosti (Waldov test stat. znaèilnosti regresijskih koeficientov);
#•	p: p vrednost za dvostranski test stat. znaèilnosti regr. koeficientov;
#•	IZ: spodnja in zgornja meja 95% intervala zaupanja za regr. koeficiente 
#•	beta: standardizirani regresijski koeficienti;
#•	kspr: kvadrirana semiparcialna korelacija;za koliko bi se koeficient determinacije zmanjšal, èe vzamemo ven ta napovednik
#•	VIF: faktor poveèanja kvadrata stand. napake (variance inflation factor);
#•	Tol.: toleranca. 1-tol dobimo % napovedi te spr. glede na vse ostale
#približno tako tabelo bi oddali v poroèilu
#priredba IZ -> v kodi najdi confint(reg_objekt,level=)

#ocene z bootstrapom; 
regzank(lmr) #Uporaba zankanja je potrebna, èe analiza kaže na odstopanja od predpostavk


#PRIMERJAVA MODELOV*********
#-	v model vkljuèimo vse N, potem po potrebi kakšnega izkljuèimo (smiselno, ko imaš majhno število N) 
#-	Èe imaš veèje število N, je zelo koristno, da na zaèetku naredimo vrstni red teoretièni pomembnosti (kateri N so na podlagi teorije bolj pomembni in kateri manj) 

lmr2 <- lm(DU~TN+PRI+VZT, data=x) #gnezden model
summary(lmr2) #manj napodevnikov, moènejši model, veèji adj. R in manjši IZ
regtabela(lmr2)

#primerjava obeh modelov
anova(lmr2,lmr)

#namesto da izloèamo, lahko hierarhièno dodajamo spr. v model
#primerjamo s tistimi, ki so bili nazadnje stat. znaèilni. Èe en vmes ni, ga spušèamo pri anovi
hm1 <- lm(DU~TN, data=x)
summary(hm1) #pogledamo p (stat. znaèilno -> veèja napovedna moè od H0)

hm2 <- lm(DU~TN+PRI, data=x)
anova(hm1,hm2)

hm3 <- lm(DU~TN+PRI+NAT, data=x)
anova(hm2,hm3)

hm4 <- lm(DU~TN+PRI+VZT, data=x)
anova(hm2,hm4)

#POSTOPNA GRADNJA MODELA********
step(lmr)
# AIC -> nižja vrednost, boljši model


# KONKRETNO NAPOVEDOVANJE
predict(regresijski objekt, podatki) #mora biti podatkovni okvir, ne matrika
#bindamo osebo, ki ji manjka odv. spremenljivka
xn <-rbind(c(100,50,NA,50,NA),x)
x1 <- rbind(xn,x) #èe hoèemo dodat v tabelo
predict(lmr,xn)

predict(lmr,xn,interval="prediction")[1,]
predict(lmr,xn,interval="confidence")[1,] #to nam pove za IZ glede na ostale osebe

#*********************************************************ISKANJE NAPAK V MODELU*******************************************
lmr <- lm(DU~TN+PRI+VZT,   data=x) )
plot(lmr)
#graf ostankov--> èe dobimo ukrivljeno rdeèo èrto zaradi skupka oseb, potem lahko da odnos ni linearen
#normalni kvantili + ostanki -> dokler je spodaj SD med 2 in 3, je okej in zankanje ni potrebno
#graf odnosa med napovedano vrednostjo in variabilnostjo --> na grafu vidimo homogenost varianc (èrta je ravna - homogenost)
#graf vplivnosti oseb -> cookova razdalja je prikazana, èe je kakšna oseba daleè od povpreèja napovedi in ima veliko napako napovedi

#raèunanje cookove razdalje
cd <- cooks.distance(lmr2) #ali so osamelci vplivne toèke

Boxplot(cd)
which.max(cd)#pove katera oseba ima najveèjo vplivnost
cd[oseba] #da dobimo cookovo razdaljo -> problematiène so vrednosti okrog 1

lmbrez <- lm(DU~TN+PRI+VZT, data=x[-9,]) #ali bi se model spremenil brez te osebe

#studentizirani ostanki
sr<-rstudent(m)
Boxplot(sr)
describe(sr)
#povpreèje ostankov mora biti 0 (mean), SD pa blizu 1. Negativna splošèenost, ni tako slaba kot poz.

#regresija s kategorialnimi napovedniki****************
#odvisna spr. mora biti vedno intervalna

x$slog <- as.factor(x$slog) #da definiramo nominalno spr.
levels(x$slog) <- c("avtoritativen","permisiven","avtoritaren")
#regresija samo za en spr. (anova)
lmr1<-lm(govor~slog, data=x)
summary(lmr1)
#ali anovarez <- aov(x$govor~x$slog), anovasum <- summary(anovarez), anovasum
#pogledamo ali je model stat. znaèilen

#estimate std. --> pove razlike v rezultatu odvisne spr., èe je x (napovedna spr.) drugaèna za 1 toèko
#prvi slog nima indikatorske spremenljivke

#èe imamo kategorialne spremenljivke lahko interpretiramo posamezne skupine;
describeBy(os$govor, group=os$slog)

#dodamo drugo spremenljivko;
lmr2 <- lm(govor~slog+pismen, data=x)
summary(lmr2)

#preverimo ali je model boljši
anova(lmr1,lmr2)

#NELINEARNI ODNOSI
#pogledamo odnos

cor(x$starost,x$govor) #pogledamo p, stat. znaèilnost nakazuje odvisnost spremenljivk
sp(govor ~ starost, data=x)
lmrneki <- lm(govor ~ starost, data=x)
#opisovanje odnosa s kvadratno fukncijo
lmr <- lm(govor ~ starost + I(starost**2), data=x)
summary(lmr)
plot(lmr)
#pojasnjevali smo moè starosti
#s tem ko smo dali na kvadrat
#stvar lahko posplošimo na populacijo
#lahko izraèunamo moè pojasnjene variance tako kot prej
s2$r.squared-s1$r.squared #vzamemo summary od 1. in 2. lm-ja torej linearnega in nelinearnega

anova(lmrneki,lmr)
plot(lmr1)

#LOGISTIÈNA REGRESIJA********************************************************************************************************

#odvisna spremenljivka je dihotomna spremenljivka
describe(x)

#korelacije lahko raèunamo z dihotomno spr. a moramo biti pozorni na to
corr.test(x)


logm <- glm(izid~vaja+anksioznost, data=x, family=binomial)
logm
summary(logm)
#teh koeficientov ne moremo direktno interpretirati
#Testiramo stat. znaèilnost z Waldovim testom
#ali z likelihood ratio test, LRT.. ali je okej èe spustimo en napovednik

drop1(logm,test="LRT") #gledamo AIC (manjši kot je, bojši je model - gleda racio med št prediktorjev in napovedno vrednotjo)
koef <- logm$coefficients
#èe želimo IZ;
IZ <- confint(logm)

#lahko združimo koeficiente in IZ;
K <- cbind(logm$coefficients,confint(logm))
#èe želimo interpretirati koeficiente;
or <- exp(K) #interpretiramo.. 1 toèka v napovedniku, pomeni za x boljsi rezultat odvisne spremenljivke (za 2 toèki veè zmnožimo možnost..)
#èe je razmerje obetov 1, N nima dodane vrednosti, pod 1 npr 0.75.. pomeni, da se obet zmanjša za 25%

#PREVERJANJE PRILEGANJA MODELA
prileg <- hoslem(x$izid,predict(logm,x,type="r"))
#dobimo prièakovane in dejanske frekvence (hi^2)
prileg #p ne sme biti stat. znaèilen


#Napovejmo 
xn<-rbind(c(NA,30,45), x)
x1 <- rbind(xn,x)
nap <- predict(logm,x1,type = "response")
nap[1] #rezultat nad 0.5 napoveduje uspešnost

#klasifikacijska tabela**************
klastabela(logm) #pove kako bolj uspešni smo z našim modelom vs H0
#iz teh % napovedi ne moremo sklepati v uspešnosti, to lahko vidimo le skozi poskus


#analiza glavnih komponent****************

describe(x)
#èe imajo spr. enake variance, imajo podobno težo. èe pa imajo razliène variance, potem se utež spreminja (vairanca pa ne odraža pomembnosti neke spr.) -> uporabljamo standardizirane vrednosti
cor(x)

#povpreèni izraèun korelacije
(sum(cor(x))-9)/72 #odštejemo diagonalo in delimo s številom korelacij

cor.plot(cor(x)) #barven prikaz povezanosti
spm(x[,1:5]) #razsevni diagram -> pogledamo ali so odnosi bolj linearni ali ne. 
#pri analizi komponent ni pomembna porazdelitev spremenljivk


gk1 <- principal (x,nfactors=) #ne potrebujemo funkcije summary
gk1 #$values, weights --> vrednosti niso interpretabilne, ker so odvisne od števila spremenjlivk

#1. komponentne uteži (utež*komponenta)
#h^2 pomeni delež pojasnjene variance, želimo nad 50%
#u^2 je nepojasnjena varianca (torej koliko razložijo druge komponente)
#SS loadings, èe seštejemo vse uteži, nam pove koliko razloži prva komponenta.. npr: s=5, prva komponenta razloži veè kakor 5 drugih komponent
#Propor var; koliko var razloži glavna komponenta

#èe nas zanima model z veè kot eno komponento
#matrièno množenje; %*%
xgk <- scale(x) %*% gk1$weights
#èeprav se uteži razlikujejo v splošni velikosti, imajo poporcionalen vzorec (èe primerjamo to matriko in gk1$weights)
#dobili smo standardizirano verzijo uteži

plot(gk1$scores,rowSums(scale(x)))
#primerjamo ujemanje vsoto standardiziranih dosežkov --> koliko je razlika, èe jih samo seštejo in kaj doprinesejo uteži (bolj razgibano pomeni, da imajo uteži veèji pomen)
#koliko komponent moramo obdržati? to je arbitrarno, gledamo na pragmatièni vidik.
#pogledamo koliko variance pojasnejo komponente
plot(gk1$values,type = "b", xlab = "komponenta", ylab = "Lastna vrednost") #relativno visoka pojasnjevalna moè
#y os predstavlja pojasnjevalno moè

gk9 <- principal(x,nfactors=9,rotate="none")
#gledamo moè komponent
#gledamo uteži, PC1,PC2.. so komponente, zdaj pa gledamo kako komponenta korelira s testi.
#ostale komponente, ne prvo, lahko interpretiramo kot nek kontrast med skupinami (rezultati so uteži)
#vidimo tudi kako so komponente sestavljenje iz posameznik spremenljivk

gk2 <- principal(x,nfactors=2,rotate="none")

gk1$communality
gk2$communality
#primerjamo, gledamo, kje je veè pojasnjene variance vseh analiziranih spremenljivk

#grafièna predstavitev nerotirane 2 komponentne
plot(gk2)
#imamo prvo in drugo komponento os. naredimo tak plot, da vidimo cel razpon
plot(gk2, xlim=c(-1,1), ylim=c(-1,1))

#uporabili bomo rotacijo, ki naj bi se dobro obnesla, pri podatkih, ki nimajo tako èiste strukture
gk2g <- principal(x, nfactors=2, rotate="geominQ")
#kaj se zgodi, èe bi rotirali vseh 9 komponent, zakaj tega v življenju ne bi želela poèet?

gk2g$weights
biplot(gk2g)
principal(x,nfactors = 3, rotate = "geominQ")$Structure

plot(gk2g, xlim=c(-1,1), ylim=c(-1,1))

gk2v <- principal(x,nfactors=2,rotate = "varimax")
#nekateri deli so enaki; pojasnjene in nepojasnjene variance 
# proportion var -> koliko pojasnjune vsaka sama, vidiš da se izide pri cumulative var
#spremeni pa se kompleksnost faktorskih uteži; "com" je kompleksnost. ena komponenta v veliki meri pojasnjuje rezultate teeh dveh testov
#ena komponenta ima obe uteži približno enaki, kar pomeni, da je kompleksnost dva in da potrebuješ oba, da lahko dobiš tako dober rezultat
#pri pravokotni rotaciji so 
#uteži - kako bi morali utežit obe komponenti, da bi dobili pravilen rezultat, hkrati dobimo pearsonov koeficient korelacije med testom in neèem

#2 meri nekoreliranih konstruktov.. 
plot(gk2v, xlim=c(-1,1), ylim=c(-1,1))
#prva komponenta še vedno v veèji meri pojasnjuje rezultat na nebesednih mestih in druga še vedno bolje pojasnjuje besedno aritmetièni del.. prisilili smo obe osi v pravokotni medsebojni položaj, torej da sta nekorelirani. 
#komponenti nista veè èisti meri ene in druge široke sposobnosti
#primerjamo
gk2g in gk2v
#Vidimo, da one meri nista veè èisti meri, ampak sta ob kontroli drugih spremenljivk, ki so tudi uporabljene v analizi
#s pravokotno rotacijo - preprostejša, ker je manj rezultatov. Matrika uteži je enaka. Je pa interpretacija lažja pri poševnokotno rotiranih komponentah
#pri pravokotni rotaciji seštejemo uteži, ki so uporabne in odštejemo tiste, ki so uporabljene pri drugi komponenti, ker ne smeta korelirati (zato veè minusov)
#druga opcija; interpretacija komponent nas ne zanima preveè - selekcijske situacije, ko želimo samo optimalen selekcijski model, torej ne rabimo razlage, samo želimo, da niso korelirane komponente

#************** novi podatki******
x <- read.table("file:///C:/Users/Uporabnik/Dropbox/Psihologija/2. letnik/1. semester/multivariatne/mvm4_impulzivnost.dat", header = T)
summary(x)
describe(x) 
spm(x)

#narediš gk1 in pogledaš plot
#prvi dve komponenti razložita veè kot ostali
#prva komponenta razloži avanturistiènost in malo SSS in druga razloži impulzivnost in malo SSS
imp2$weights
# vidimo da so variance (h2) dobro pojasnjene samo pri prvih 6ih spremenljivkah. komponenti torej nista najboljši, sam z njime ne moremo zares razložiti SSS-a
#zato bomo pogledali tri komponentno rešitev in jo rotirali
imp3 <- principal(x,nfactors = 3,rotate = "oblimin")
#vidimo da tretja komponenta razloži SSS, tako imamo enostavnejšo interpretacijo

imp3$weights
#s temi komponentami smo bolj jasno in enoznaèno opisali spremenljivke


#********************* EKSPLORATORNA FAKTORSKA ANALIZA**************

#do zdaj smo delali z latentnimi spremenljivkami. s Faktorsko analizo poskušamo raziskovati latentne sprem.

#delamo na bfq podatkih
spm(x[,5:9])

corr.test(x[,5:9])
#vsi pari so pozotivno korelirani, med nizko do srednje visoke
#verjentosti so tako nizke, da jih napiše kot 0

(sum(cor(x[,5:9]))-5)/20

#s faktorsko analizo bomo poskuušali pojasnit variabilnost spremenljivk
describe(x)
#porazdelitev ni èisto reprezentativna, po spolu je vzorec uravnotežen, povpreèni rezultati bfq so okoli 80 toèk, variabilnost je med spremenljivkami podobne. Te so potencialno pomembno samo èe uporabimo kakšno metodo, ki ni odvisna od merske lestvice
#zanima nas obblika porazdelitve: metoda najceèjega verjetja zahteva približno normalno porazdelitev
qqPlot(x$sp)
#mislili smo, da je odstopanje zaradi dveh oseb, zgleda da jih je malo veè, a vseeno ni tako zelo veliko odstopanje

#naredili bomo faktorsko analizo:
fa1 <- fa(x[,5:9], nfactors = 1, fm="ml")
#rezultati so na videz podobni komponentni analizi
#dobimo uteži (vektor) ter komuniteto (pojasnjeno) in u2 nepojasnjeno varianco

# najprej imamo hikvadrat test, ima veèjo moè pri veèjih vzorcih.. manjši kot je, težje bomo našli odstopanja. 
#dobili smo hi=31,7, p < 0.001. Torej z enim faktorjem nikoli ne bi dobili tako velikih razlik med dejansko in predvidenimi rezultati
#potem bomo imeli indeks prileganja: RMSEA in TLI
#RMSEA je < 0.06, TLI
#pomembni so še ostanki: razlike med dejanskimi in ocenjenimi korelacijami spremenljivk

round(fa1$residual,3)
.68*.72 #približen kriterij katera odstopanja so velika, torej pogledamo te korelacije in prejšnje
#za nas je pomembno tole: The degrees of freedom for the model are 5  and the objective function was  0.06 

# veè uteži kot obdržimo, manjše število prostostnih stopenj imamo (ne sme biti negativno!)
#pri 5ih spr. lahko identificiramo najveè 5 faktorjev (komponent)

fa_bfq2r <- fa(x[,5:9], nfactors = 2, fm="ml", rotate= "oblimin", n.iter = 1000)#rotiramo
#opozorilo, da so rezultati izven norme



#***********************************************************************************************************
  #podatki; èustva
describe(x)

èus_vzp <- fa.parallel(x, fm= "minres", fa="fa", n.iter= 1000, sim=F)
cbind(èus_vzp$fa.values, èus_vzp$fa.simr)
#pri [,2] moramo premaknit ocene za eno v levo (0,...)
#obsržimpo samo prvo dva faktorja, ker ju ne moremo loèiti od napake
#kerimamo dokaj majhen vzorec, moramo zraèunati IZ, da izraèunamo vrednost uteži

fa_èus <- fa(x, nfactors=2, fm="minres", rotate="promax", n.iter=1000)
#èe je com okoli 1 je okej
#hvaležnost ima najvišji h2-> najveèje pojasnjena varianca
#RMSR 0.5 prileganje ni najboljše, je pa zadoviljivo

round(fa_èus$residual, 2)
#naš vzorec je majhen, zaot lahko vejreteno identificiramo samo 2 faktorja
#po rotaciji se naše spr. še zmeraj razlikujejo po kompleksnosti.. torej eno spr. lahko bolj ali manj pojasni samo en faktor
# Zveselje= 0.92*f1+0.07*f2+e

#pogledamo kateri so najbolj èisti kazalci prvega foaktorja, spoštovanje, sposštovanje, hvaležnost ljubezen, navezenost, torej visok 1 faktor in varianco? prvi faktor lahko poimenujemo pozitivno èustvovanje
#drugi ima manj èistih kazalnikov (da ima visoko utež samo na drugem fkstorju), strah, žalost, negotovost
#imamo veliko visokih faktorskih uteži, zsto se te tudi bolj natanèno doloèene 

#faktorski dosežek - empirièna ocena dosežka na faktorju
#vsake spr. vsebuje tudi lastni faktor, del ki ga n edeli z nobeno drugo spr. zato ne moremo nikoli popolnoma natanèno izraèunati oceno osebe ma doloèenem faktorju
#lahko pa jih ocenimo - ko so faktorske uteži visoke in ko imamo dovolj visoke uteži na posamezen faktor
#korelacija med faktor in reg. ocenami - obe korelaviji sta zelo visoki

fa_èustva <-fa(nfactors =2, fm="minres", rotate="promax", scores= "regression")
fa_èustva$weights #dobimo faktorske uteži
cor(fa_èustva$scored)
#to je eksplorastorna faktorska naaliza, to more biti prvi korakt pri raziskovsnju konstrukta, da dobimo neke hipotze o latentni strukturi, da jih potem lahko preverjamo naprej




#*********ANALIZA MANJKAJOÈIH PODATKOV******
#trije tipi manjkajoèih podatkov:
#MCAR - manjkanje popolnoma po sluèaju, vzrok je povsem ne povezan s tistim, kar merimo (npr pomanjkanje pozornosti.. samo kar merimo nesme biti povezano z merjenjem tega)
#MAR - sluèajno --> verjetnost manjkanja je povezana z ravnjo nekaterih spremenljivk (spr ni kriva za nastanek manjkajoèe vrednosti, sta pa v povezavi)
#MNAR - ne sluèajno manjkanje (ankete javnega mnenja, volivci doloèenih strank pogosto zavrnejo sodelovanje v anketi)

x <- read.table("file:///C:/Users/Uporabnik/Dropbox/Psihologija/2. letnik/1. semester/multivariatne/AMP.txt", header=T)
> View(x)
d <- x[,13:22]
#za vsako osebo lahko preštejo št. manjkajoèih vrednosti. Problem je, da se jih ne moremo znebiti, ker potem bi bili brez oseb
is.na(d)
xp <- rowSums(is.na(d))
table(xp)
sum(xp)
dbm <-na.omit(D)
# ohranili bomo osebe, ki imajo manj kot x manjkajoèih vrednosti --> funkcija which
d1 <- d[which(xp<5),]

#zanima nas kako dobro lahko ocenimo dejanske korelacije brez manjkajoèih vrednost, v pogojih kjer imamo manjkajoèe vrednosti, a so te nastale iz razliènih vzrokov
# imamo dva naèina za raèunanje korelacij

#osnovna korelacijska matrika
R <- cor(x[,1:3])

#MCAR
#OCENILI jih bomo tako, da bomo izbrisali osebe z manjkajoèimi podatki (2 opciji)

#Naredimo nov podatkovni okvir
xmcar <- x[,4:6]
cor(na.omit(xmcar)) #ali
cor(xmcar, use="complete.obs") #povemo, da raèuna samo osebe s popolnimi podatki. Da bo bolj pregledno bomo odšteli osnovno korelacijsko matriko, da vidimo napako
round(cor(xmcar, use="complete.obs") - R,2)

#pomagali si bomo z algoritmi --> vstaljali bomo NA vrednosti
#nekateri zahtevajo specifiène porazdelitve, ta ki ga bomo uporabili jih ne, lahko uporabljamo kategorialne in nominalne spr. 
#edini obvezni argument je podatkovna matrika, ki vsebuje spr. s popolnimi in z manjkajoèimi vrednostmi
forx <- missForest(xmcar)
#dobimo seznam, ki vsebuje: dve stvari
forx$OOBerror #napaka
View(forx$ximp)
round(cor(forx$ximp) -R, 2)
#tako z vstavljanjem kot z analizo vseh podatkov, smo dobili prib. enake korelacije
# ko manjkajo po sluèajo podatki, jih z vstavljanjem ne bomo hudo izkrivili. izgubili bomo neko informacijo, a ni pa sistematiènega izkrivljanja

#kaj pa èe podatki ne manjkajo po sluèajo? Npr. IQ test, ali je manjkajoè podatek povezan z inteligentnostjo? ali od znanja (torej èe kontroliramo znanje, bodo imeli enako inteligentni uèenci enako vrednost manjkanja)

#MAR

xmar <- x[,7:9]
cor(na.omit(xmar)) #ali
cor(xmar, use="complete.obs") #povemo, da raèuna samo osebe s popolnimi podatki. Da bo bolj pregledno bomo odšteli osnovno korelacijsko matriko, da vidimo napako
round(cor(xmar, use="complete.obs") - R,2)
# najveèje spremembe so pr cor kjer sploh ni bilo NA vrednosti, a je nanje vplivalo brisanje oseb
forx1 <- missForest(xmar)
forx1$OOBerror #napaka
View(forx1$ximp)
round(cor(forx1$ximp) -R, 2)
# bolj se splaèa uporabiti missforest

#MNAR

#manjkajo vrednosti 1/10 najbolj inteligentnih. Èetudi kontroliramo druge spr. se tu NA vrednosti ne bodo spremenile

xmnar <- x[,4:6]
cor(na.omit(xmnar)) #ali
cor(xmnar, use="complete.obs") #povemo, da raèuna samo osebe s popolnimi podatki. Da bo bolj pregledno bomo odšteli osnovno korelacijsko matriko, da vidimo napako
round(cor(xmnar, use="complete.obs") - R,2)
forx2 <- missForest(xmnar)
forx2$OOBerror #napaka
View(forx2$ximp)
round(cor(forx2$ximp) -R, 2)
#napaka je nezanemarljiva, èe imamo nesluèajno manjkanje imamo problem. Gledati moramo z rezervo


#DELAMO NEKEJ
as.factor # nominalna
as.ordered # ordinalna

#naše spr jemljejo kot ordinalne, ker je 1 kot strinjanje veè kot 0
d$V1 <- as.ordered (d$V1) #uporabili bomo apply
dk <- as.data.frame(apply(d,2, as.ordered)) # 2. argument: 1 funkcijo uporabimo na vrsticah, 2 na stolpcih
dv <- missForest(dk,variablewise=T)
dv$OOBerror # proti 0 smo veèino pravilno razvrstili
View(dv$ximp)

#vèasih želimo dobiti napako za vsako spr. posebaj, èe imajo veliko razliko v št NA vrednosti, ali pa imajo drugaè var razmik
#zato uporabimo dodatni argument variablewise=T





























#***************************************** TESTNA TEORIJA *************************************************************************************************



X0 <- read.delim("clipboard")
X <- X0[,c(1,12,13,2:11)]
names(X) <- c("Sifra", "SpolM", "Starost",paste0("V_",1:10) )
X$SpolM <- 2 - X$SpolM
Xp <- X[,4:13]
Xp[Xp==0]<-NA
Xp[,c(3,5,8:10)] <- 5-Xp[,c(3,5,8:10)]
X[,4:13]<-Xp

#izbor oseb
table(X$Starost)
Xrezerva <- X
X <- X[(X$Starost)>=30&(X$Starost<=35),]

#pregled podatkov
apply(X[,-1],2,table)
describe(X)
which(X$SpolM>1)

#nemogoce vrednosti
X$V_9[X$V_9<0] <- NA #niclam dopisemo NA
X <-scrub(X,where=c(2,4:13), min=c(0,rep(1,10)), max=c(1, rep(4,10)))
# rep --> kaj ponovi veckrat, rep(2,5) ponovi 2 5x
mv <- rowSums(is.na(X))
sum(mv)/12000
table(mv)

Xbm <- X[which(mv<10),]
#kako bomo obravnavali spremenljivke? kot strogo kategorialne? ali kot stevilske vrednosti. potem ne bodo cele vrednosti 
Xbm$SpolM <- as.factor(Xbm$SpolM)
set.seed(2017)
vstavljene <- missForest(Xbm[,-1], variablewise = T)
#obravnavali jih bomo kot stevilske, ker to predpostavlja testna paradigma, in ce bi jih obravnavali kategorialno potem bi izgubili veliko infomacij o visini?
vstavljene$OOBerror
imena <- names(Xbm)

Xbm <- cbind(Xbm$sSifra, vstavljene$ximp)
names(Xbm) <- imena
apply(Xbm[,-1],2,table)

write.csv2(Xbm, "RSESbm.csv")


#########################################
Xbm$ktd <- rowSums(Xbm[,4:13])

