#sletter alle variabler
rm(list=ls())

#Kode for Ã¥ jobbe i riktig mappe
setwd("~/Universitetet/Bacheloroppgave")


#### Del 1 : NÃ¥r er strÃ¸mmen billigst i lÃ¸pet av Ã¥ret?
 
#Lagrer data fra den daglige strÃ¸mprisen i NO2 i 2021 som variabel
daily2021=read.csv2("elspot-prices_2021_daily_nok.csv")
head(daily2021)
plot(daily2021$Nummer,daily2021$Kr.sand, xlab = "Dager etter nyttÃ¥r", ylab="Spotpris", main="Spotpris i NO2")
daily2021$Aarstid=as.factor(daily2021$Aarstid) #Gjør Årstid om til kategorisk variabel
library(ggplot2)
ggplot(daily2021, aes(x=Nummer, y=Kr.sand)) +
  geom_point() +
  geom_point(aes(color = Aarstid)) +
  labs(title = "Strømpris NO2", x = "Dager etter nyttår", y = "Pris i NOK per MWh", color = "Årstid")+
  scale_color_manual(labels = c("Vinter", "Vår","Sommer", "Høst"), values = c("blue", "red", "green", "orange"))


#Lager modeller for spotpris i NO2 med data fra 2021
model0= lm(Kr.sand~Helg+Helligdag+Helg:Aarstid+Aarstid+Fellesferie,data=daily2021)

summary(model0)


#PrÃ¸ver med log(Kr.sand) siden økonomiske modeller ofte funker bedre slik
model1 = lm(log(Kr.sand)~Helg+Helligdag+Aarstid+Helg:Aarstid+Fellesferie,data=daily2021)
summary(model1)


#Finner beste delmengde av dummy-variabler
library(leaps)
bestSubset=regsubsets(log(Kr.sand)~Helg+Helligdag+Aarstid+Helg:Aarstid+Fellesferie,method="forward",data=daily2021)
summary(bestSubset)
summary=summary(bestSubset)
summary$adjr2
summary$cp
summary$bic
par(mfrow=c(2,2))
plot(summary$adjr2, col="blue", xlab="antall prediktorer", ylab="Adjusted R-squared")
plot(summary$cp, col="green", xlab="antall prediktorer", ylab="Cp")
plot(summary$bic, col="red", xlab="antall prediktorer", ylab="Bic")
dev.off()

model2=lm(log(Kr.sand)~Helg+Aarstid,data=daily2021)
summary(model2)


#Fjerner kolonnene for helligdag og ferie
daily2021=daily2021[,-c(21,23)]
head(daily2021)
daily2021

#Henter data fra flere Ã¥r for Ã¥ fÃ¥ et stÃ¸rre datagrunnlag
daily2020=read.csv2("elspot-prices_2020_daily_nok.csv")
head(daily2020)
daily2years=rbind(daily2020, daily2021)
head(daily2years)
model3=lm(log(Kr.sand)~Helg+Aarstid+Aar,data=daily2years)
summary(model3)
daily2years$Aar=as.factor(daily2years$Aar)
ggplot(daily2years, aes(x=Nummer, y=Kr.sand, color=Aar)) +
  geom_point()

daily2019=read.csv2("elspot-prices_2019_daily_nok.csv")
head(daily2019)
daily2018=read.csv2("elspot-prices_2018_daily_nok.csv")
daily2017=read.csv2("elspot-prices_2017_daily_nok.csv")
daily5years=rbind(daily2017,daily2018,daily2019,daily2years)

#Modeller med data fra 5 siste Ã¥r
model4=lm(log(Kr.sand)~Helg+Aarstid+Aar,data=daily5years)
summary(model4)
model5=lm(log(Kr.sand)~Helg+Aarstid,data=daily5years)
summary(model5)
plot(daily5years$Nummer,log(daily5years$Kr.sand), xlab = "Dager etter nyttÃ¥r", ylab="log(Spotpris)", main="Spotpris i NO2")
ggplot(daily5years, aes(x=Nummer, y=Kr.sand, color=Aar)) +
  geom_line() +
  labs(title = "Strømpris NO2", x = "Dager etter nyttår", y = "Pris i NOK per MWh", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
ggplot(daily5years, aes(x=Nummer, y=log(Kr.sand), color=Aar)) +
  geom_line() +
  labs(title = "Strømpris NO2", x = "Dager etter nyttår", y = "log(Pris i NOK per MWh)", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))


#PrÃ¸ver uten 2021 siden prisutviklingen i 2021 er forskjellig fra de andre Ã¥rene
daily4years=rbind(daily2017,daily2018,daily2019,daily2020)
daily4years$AarF=as.factor(daily4years$Aar)
daily4years$Aarstid=as.factor(daily4years$Aarstid)

model6=lm(log(Kr.sand)~Helg+Aarstid+AarF,data=daily4years)
summary(model6)
model7=lm(log(Kr.sand)~Aarstid+Helg,data=daily4years)

summary(model7)


#Normalfordelte residualer?
par(mfrow=c(2,2))
qqnorm(log(daily2021$Kr.sand), main="Qqnorm-plott strømpris 2021")
qqnorm(log(daily5years$Kr.sand), main="Qqnorm-plott strømpris 2017-2021")
qqnorm(residuals(model2), main="Qqnorm-plott residualer figur 2")
qqnorm(residuals(model4), main="Qqnorm-plott residualer figur 4")
hist(log(daily2021$Kr.sand),main="Histogram 2021")
hist(log(daily5years$Kr.sand),main="Histogram 2017-2021")
hist(residuals(model2),main="Histogram residualer figur 2")
hist(residuals(model4),main="Histogram residualer figur 4")
par(mfrow=c(1,2))
plot(residuals(model2), data=daily2021, type="l", main="Tidserieplott 2021", xlab="Dager etter 01.01.21")
plot(residuals(model4), data=daily5years, type="l", main="Tidserieplott 2017-2021", xlab="Dager etter 01.01.17")
dev.off()

#Prediksjon for 2022 ved bruk av modeller i del 1
daily2022=read.csv2("daily2022Tid.csv")
daily2022$Aarstid=as.factor(daily2022$Aarstid)
daily2022Strom=read.csv2("elspot-prices_2022_daily_nok.csv")


pred2022_1=exp(predict(model2, newdata=daily2022))
intPred2022_1=exp(predict(model2, newdata=daily2022, interval="prediction", level = 0.95))
intPred2022_1
plot(daily2022$Nummer,pred2022_1, main="Modell for 2022 med data fra 2021",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 3000), type="l")
lines(daily2022$Nummer, intPred2022_1[,2], col="blue", lty=2)
lines(daily2022$Nummer, intPred2022_1[,3], col="blue", lty=2)
lines(1:72, daily2022Strom$Kr.sand, col="orange", lty=2)


pred2022_2=exp(predict(model5, newdata=daily2022))
intPred2022_2=exp(predict(model5, newdata=daily2022, interval="prediction", level = 0.95))
plot(daily2022$Nummer,pred2022_2, main="Modell for 2022 med data fra 2017-2021",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 3000), type="l")
lines(daily2022$Nummer, intPred2022_2[,2], col="blue", lty=2)
lines(daily2022$Nummer, intPred2022_2[,3], col="blue", lty=2)
lines(1:72, daily2022Strom$Kr.sand, col="orange", lty=2)

#### Del 2: Legger til informasjon om andre typer variabler


#Laster inn data
overforing2021=read.csv2("exchange-no-connections_2021_daily.csv")
overforing2020=read.csv2("exchange-no-connections_2020_daily.csv")
overforing2019=read.csv2("exchange-no-connections_2019_daily.csv")
overforing2018=read.csv2("exchange-no-connections_2018_daily.csv")
overforing2017=read.csv2("exchange-no-connections_2017_daily.csv")
forbruk2021=read.csv2("consumption-no-areas_2021_daily.csv")
forbruk2020=read.csv2("consumption-no-areas_2020_daily.csv")
forbruk2019=read.csv2("consumption-no-areas_2019_daily.csv")
forbruk2018=read.csv2("consumption-no-areas_2018_daily.csv")
forbruk2017=read.csv2("consumption-no-areas_2017_daily.csv")
magasin2021=read.csv2("hydro-reservoir_2021_weekly.csv")
magasin2020=read.csv2("hydro-reservoir_2020_weekly.csv")
magasin2019=read.csv2("hydro-reservoir_2019_weekly.csv")
magasin2018=read.csv2("hydro-reservoir_2018_weekly.csv")
magasin2017=read.csv2("hydro-reservoir_2017_weekly.csv")
vinddata=read.csv2("Vinddata.csv")
nedborOgTempData=read.csv2("nedborOgTempData.csv")

daily2021$Magasin=magasin2021$NO
daily2021$Forbruk=forbruk2021$NO2
daily2021$Overforing=overforing2021$NO...DK+overforing2021$NO...DE+overforing2021$NO...NL+
  overforing2021$NO...UK-overforing2021$NO1...NO2-overforing2021$NO5...NO2
daily2021$Vind=vinddata$Middel.av.middelvind.fra.hovedobs...dÃ.gn.[1462:1826]
daily2021$Nedbor=nedborOgTempData$NedbÃ.r.siste.30.dÃ.gn[1493:1857]
daily2021$Temperatur=nedborOgTempData$Gjennomsnittstemperatur[1493:1857]


#LineÃ¦r modell for 2021
head(daily2021)
model11=lm(log(Kr.sand)~Helg+Aarstid+Overforing+Vind+Nedbor+Temperatur+Forbruk+Magasin+
             Aarstid:Temperatur,data=daily2021)
summary(model11)
model12=lm(log(Kr.sand)~Aarstid+Overforing+Vind+Nedbor+Temperatur+Forbruk+Magasin+
             Aarstid:Temperatur,data=daily2021)
summary(model12)

#Med nedbør og temperatur siste 7 dager som prediktor
daily2021$Nedbor=nedborOgTempData$NedbÃ.r.siste.7.dÃ.gn[1493:1857]
daily2021$Temperatur=nedborOgTempData$Gjennomsnittstemperatur.siste.7.dÃ.gn[1493:1857]
model12=lm(log(Kr.sand)~Aarstid+Overforing+Vind+Nedbor+Temperatur+Forbruk+Magasin+
             Aarstid:Temperatur,data=daily2021)
summary(model12)

#Tilbake til standard
daily2021$Nedbor=nedborOgTempData$NedbÃ.r.siste.30.dÃ.gn[1493:1857]
daily2021$Temperatur=nedborOgTempData$Gjennomsnittstemperatur[1493:1857]
model12=lm(log(Kr.sand)~Aarstid+Overforing+Vind+Nedbor+Temperatur+Forbruk+Magasin+
             Aarstid:Temperatur,data=daily2021)
summary(model12)

#Korrelasjon mellom variablene
library(GGally)
ggpairs(daily2021[c(10,23:27)])

#GAM for 2021
library(gam)
gam1 = gam(log(Kr.sand)~Helg+Aarstid+s(Overforing)+s(Vind)+s(Nedbor)+s(Temperatur)+s(Forbruk)+s(Magasin)
           ,data=daily2021)
summary(gam1)

#Modell for de fem siste Ã¥rene
daily2021=daily2021[,-c(26,27,28)]
head(daily2021)
daily2020$Overforing=overforing2020$NO...DK+overforing2020$NO...DE+overforing2020$NO...NL+
  overforing2020$NO...UK-overforing2020$NO1...NO2-overforing2020$NO5...NO2
daily2019$Overforing=overforing2019$NO...DK+overforing2019$NO...DE+overforing2019$NO...NL+
  overforing2019$NO...UK-overforing2019$NO1...NO2-overforing2019$NO5...NO2
daily2018$Overforing=overforing2018$NO...DK+overforing2018$NO...DE+overforing2018$NO...NL+
  overforing2018$NO...UK-overforing2018$NO1...NO2-overforing2018$NO5...NO2
daily2017$Overforing=overforing2017$NO...DK+overforing2017$NO...DE+overforing2017$NO...NL+
  overforing2017$NO...UK-overforing2017$NO1...NO2-overforing2017$NO5...NO2
daily2020$Forbruk=forbruk2020$NO2
daily2019$Forbruk=forbruk2019$NO2
daily2018$Forbruk=forbruk2018$NO2
daily2017$Forbruk=forbruk2017$NO2
daily2020$Magasin=magasin2020$NO
daily2019$Magasin=magasin2019$NO
daily2018$Magasin=magasin2018$NO
daily2017$Magasin=magasin2017$NO
daily5years=rbind(daily2017,daily2018,daily2019,daily2020,daily2021)

daily5years$Vind=vinddata$Middel.av.middelvind.fra.hovedobs...dÃ.gn.
daily5years$Nedbor=nedborOgTempData$NedbÃ.r.siste.30.dÃ.gn[32:1857]
daily5years$Temperatur=nedborOgTempData$Gjennomsnittstemperatur[32:1857]
daily5years$AarF=as.factor(daily5years$Aar)
model13=lm(log(Kr.sand)~Helg+Aarstid+Overforing+Vind+Nedbor+Forbruk+Magasin+Temperatur+
             Temperatur:Aarstid+AarF,data=daily5years)
summary(model13)
model14=lm(log(Kr.sand)~Aarstid+Overforing+Vind+Nedbor+Forbruk+Magasin+Temperatur+
             Temperatur:Aarstid+AarF,data=daily5years)
summary(model14)

model15=lm(log(Kr.sand)~Helg+Aarstid+Overforing+Vind+Nedbor+Forbruk+Magasin+Temperatur+
             Temperatur:Aarstid,data=daily5years)
summary(model15)


#Plot av prediktorer

plot(1:1826,nedborOgTempData$NedbÃ.r..dÃ.gn.[32:1857])
daily5years$Aar=as.factor(daily5years$Aar)
ggplot(daily5years, aes(x=1:1826, y=Nedbor, color=Aar)) +
  geom_point()+
  labs(title = "Plott av sum nedbør siste 30 dager", x = "Dager etter nyttår 2017", y = "Nedbør", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
sum(daily5years$Nedbor[1:365])
sum(daily5years$Nedbor[366:730])
sum(daily5years$Nedbor[731:1095])
sum(daily5years$Nedbor[1096:1461])
sum(daily5years$Nedbor[1462:1826])


ggplot(daily5years, aes(x=1:1826, y=Overforing, color=Aar)) +
  geom_point()+
  labs(title = "Plott av overføring", x = "Dager etter nyttår 2017", y = "Overføring", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
sum(daily5years$Overforing[1:365])
sum(daily5years$Overforing[366:730])
sum(daily5years$Overforing[731:1095])
sum(daily5years$Overforing[1096:1461])
sum(daily5years$Overforing[1462:1826])


plot(1:1826,daily5years$Vind)
ggplot(daily5years, aes(x=1:1826, y=Vind, color=Aar)) +
  geom_point()+
  labs(title = "Plott av vindstyrke", x = "Dager etter nyttår 2017", y = "Vindstyrke", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
sum(daily5years$Vind[1:365])
sum(daily5years$Vind[366:730])
sum(daily5years$Vind[731:1095])
sum(daily5years$Vind[1096:1461])
sum(daily5years$Vind[1462:1826])

plot(1:1826,daily5years$Forbruk)
ggplot(daily5years, aes(x=1:1826, y=Forbruk, color=Aar)) +
  geom_point()+
  labs(title = "Plott av strømforbruk", x = "Dager etter nyttår 2017", y = "Forbruk", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
sum(daily5years$Forbruk[1:365])
sum(daily5years$Forbruk[366:730])
sum(daily5years$Forbruk[731:1095])
sum(daily5years$Forbruk[1096:1461])
sum(daily5years$Forbruk[1462:1826])

plot(1:1826,daily5years$Magasin)
ggplot(daily5years, aes(x=1:1826, y=Magasin, color=Aar)) +
  geom_point()+
  labs(title = "Plott av sum nedbør siste 30 dager", x = "Dager etter nyttår 2017", y = "Nedbør", color = "Årstall")+
  scale_color_manual(labels = c("2021", "2020","2019", "2018", "2017"), values = c("blue", "red", "green", "orange", "yellow"))
sum(daily5years$Magasin[1:365])
sum(daily5years$Magasin[366:730])
sum(daily5years$Magasin[731:1095])
sum(daily5years$Magasin[1096:1461])
sum(daily5years$Magasin[1462:1826])


#GAM for de siste 5 Ã¥rene
gam2= gam(log(Kr.sand)~Helg+Aarstid+s(Overforing)+s(Vind)+s(Nedbor)+s(Temperatur)+s(Forbruk)+s(Magasin)
          ,data=daily5years)
summary(gam2)


#Normalfordelte residualer?
par(mfrow=c(2,2))
qqnorm(log(daily2021$Kr.sand))
qqnorm(log(daily5years$Kr.sand))
qqnorm(residuals(model12))
qqnorm(residuals(model13))
qqnorm(residuals(model14))
plot(residuals(model12),data=daily2021)
plot(residuals(model13),data=daily5years)
plot(residuals(model14),data=daily5years)
dev.off()


#VIF
library(car)
vif(model12)
vif(model14)
vif(gam1)
vif(gam2)


#Se hvor bra modellene funker paa framtidig data
daily2022=read.csv2("elspot-prices_2022_daily_nok.csv")
overforing2022=read.csv2("exchange-no-connections_2022_daily.csv")
forbruk2022=read.csv2("consumption-no-areas_2022_daily.csv")
magasin2022=read.csv2("hydro-reservoir_2022_weekly.csv")
vinddata2022=read.csv2("Vinddata2022.csv")
nedborOgTempData2022=read.csv2("nedborOgTempData2022.csv")

daily2022$Magasin=magasin2022$NO
daily2022$Forbruk=forbruk2022$NO2
daily2022$Overforing=overforing2022$NO...DK+overforing2022$NO...DE+overforing2022$NO...NL+
  overforing2022$NO...UK-overforing2022$NO1...NO2-overforing2022$NO5...NO2
daily2022$Vind=vinddata2022$Middel.av.middelvind.fra.hovedobs...dÃ.gn.
daily2022$Nedbor=nedborOgTempData2022$NedbÃ.r.siste.30.dÃ.gn[32:103]
daily2022$Temperatur=nedborOgTempData2022$Gjennomsnittstemperatur[32:103]
daily2022$Aarstid=as.factor(daily2022$Aarstid)

pred2022_2=exp(predict(model2, newdata=daily2022))
MSEmodel2=mean(pred2022_2-daily2022$Kr.sand)^2
pred2022_3=exp(predict(model5, newdata=daily2022))
MSEmodel5=mean(pred2022_3-daily2022$Kr.sand)^2
pred2022_4=exp(predict(model7, newdata=daily2022))
MSEmodel7=mean(pred2022_4-daily2022$Kr.sand)^2
pred2022_5=exp(predict(model12, newdata=daily2022))
MSEmodel12=mean(pred2022_5-daily2022$Kr.sand)^2
pred2022_6=exp(predict(model15, newdata=daily2022))
MSEmodel15=mean(pred2022_6-daily2022$Kr.sand)^2
pred2022_7=exp(predict(gam1, newdata=daily2022))
MSEgam1=mean(pred2022_7-daily2022$Kr.sand)^2
pred2022_8=exp(predict(gam2, newdata=daily2022))
MSEgam2=mean(pred2022_4-daily2022$Kr.sand)^2

data.frame("Modell" = c("Figur 6.2", "Figur 6.5", "Figur 6.13", "Figur 6.15", "Gam 2021", "Gam 2017-2021"),
           "MSE" = c(MSEmodel2, MSEmodel5, MSEmodel12, MSEmodel15, MSEgam1, MSEgam2) 
      )

pred2022_1=exp(predict(model12, newdata=daily2022))
intPred2022_1=exp(predict(model12, newdata=daily2022, interval="prediction", level = 0.95))
intPred2022_1
plot(1:72,pred2022_1, main="Prediksjon strømpris 2022",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 3500), type="l")
lines(1:72, intPred2022_1[,2], col="blue", lty=2)
lines(1:72, intPred2022_1[,3], col="blue", lty=2)
lines(1:72, daily2022$Kr.sand, col="orange", lty=1)


daily2022$Aarstid[60:72]=0
pred2022_2=exp(predict(model12, newdata=daily2022))
intPred2022_2=exp(predict(model12, newdata=daily2022, interval="prediction", level = 0.95))
plot(1:72,pred2022_2, main="Prediksjon strømpris 2022",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 4000), type="l")
lines(1:72, intPred2022_2[,2], col="blue", lty=2)
lines(1:72, intPred2022_2[,3], col="blue", lty=2)
lines(1:72, daily2022$Kr.sand, col="orange", lty=1)

daily2022$Aarstid[60:72]=1
pred2022_3=exp(predict(model15, newdata=daily2022))
intPred2022_3=exp(predict(model15, newdata=daily2022, interval="prediction", level = 0.95))
plot(1:72,pred2022_3, main="Prediksjon strømpris 2022",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 3500), type="l")
lines(1:72, intPred2022_3[,2], col="blue", lty=2)
lines(1:72, intPred2022_3[,3], col="blue", lty=2)
lines(1:72, daily2022$Kr.sand, col="orange", lty=1)


daily2022$Aarstid[60:72]=0
pred2022_4=exp(predict(model15, newdata=daily2022))
intPred2022_4=exp(predict(model15, newdata=daily2022, interval="prediction", level = 0.95))
plot(1:72,pred2022_4, main="Prediksjon strømpris 2022",
     ylab="Strømpris i NOK per MWh", xlab="Dager etter 01.01.22",  ylim=c(0, 4000), type="l")
lines(1:72, intPred2022_4[,2], col="blue", lty=2)
lines(1:72, intPred2022_4[,3], col="blue", lty=2)
lines(1:72, daily2022$Kr.sand, col="orange", lty=1)


#Se hvordan endring i prediktorene påvirker modellen
summary(model12)
faktorOverforing=exp(sum(daily5years$Overforing[1462:1826])
    *1/365
    *(-3.4*10^(-6))
    )
faktorOverforing^1.5/faktorOverforing*100-100
faktorOverforing^2/faktorOverforing*100-100
faktorForbruk=exp(sum(daily5years$Forbruk[1462:1826])
                  *1/365
                  *1.042*10^-5
                  )
faktorForbruk^1.1/faktorForbruk*100-100
faktorForbruk^1.5/faktorForbruk*100-100
