# Szeregi Czasowe - Projekt

# a) krótkie wprowadzenie ######################################################

#Instalacja potrzebnych bibliotek
install.packages("expsmooth") 
install.packages("forecast") 
install.packages("lubridate")

#Wczytanie potrzebnych bibliotek
library(forecast)
library(dplyr)
library(expsmooth)
library(lubridate)

#Ustawienie katalogu głownego
getwd()
setwd("C:/Users/Sebastian/Desktop/studia/II rok 2 semestr/Szeregi_czasowe_PROJEKT")

#Wczytanie plikow csv
sumagot <- read.csv("BOGMBASE.csv")
zawod <- read.csv("LNU01300000.csv")

#stworzenie szeregow czasowych
sumagot_ts<-ts(data = sumagot$BOGMBASE,start=decimal_date(ymd("1959-01-01")),frequency = 12)
zawod_ts<-ts(data = zawod$LNU01300000,start=decimal_date(ymd("1948-01-01")),frequency = 12)

# b) Poznane wykresy ###########################################################

#Wykresy

options(scipen=999) #Wyłączenie notacji wykładniczej

#Wykresy liniowe
plot(sumagot_ts,type = "l",main = "Suma gotówki w obiegu i rezerw banków komercyjnych",xlab="Rok", ylab="Suma Gotówki w MLN Dolarów")
plot(zawod_ts,type = "l",main = "Współczynnik aktywności zawodowej", ylab = "Procenty", xlab="Rok")

#Wykresy pudełkowe
boxplot(sumagot_ts~cycle(sumagot_ts),main = "Suma gotówki w obiegu i rezerw banków komercyjnych",xlab="Miesiąc w roku", ylab="Suma Gotówki w MLN Dolarów")
boxplot(zawod_ts~cycle(zawod_ts),main = "Współczynnik aktywności zawodowej", ylab = "Procenty", xlab="Miesiąc w roku")

#Wykres miesięczny
monthplot(sumagot_ts,main = "Suma gotówki w obiegu i rezerw banków komercyjnych",xlab="Miesiąc w roku", ylab="Suma Gotówki w MLN Dolarów")
monthplot(zawod_ts,main = "Współczynnik aktywności zawodowej", ylab = "Procenty", xlab="Miesiąc w roku")

#Wykres rozrzutu
lag.plot(sumagot_ts, lags=12, do.lines = FALSE,main = "Suma gotówki w obiegu i rezerw banków komercyjnych") 
lag.plot(zawod_ts, lags=12, do.lines = FALSE, main = "Współczynnik aktywności zawodowej")

#Wykresy acf
acf(sumagot_ts, main="Suma gotówki w obiegu i rezerw banków komercyjnych")
acf(zawod_ts, main="Współczynnik aktywności zawodowej")

#Wykrecy pacf
pacf(sumagot_ts, main="Suma gotówki w obiegu i rezerw banków komercyjnych")
pacf(zawod_ts, main="Współczynnik aktywności zawodowej")

#Dokładam wykres tsdipslay
tsdisplay(sumagot_ts, main="Suma gotówki w obiegu i rezerw banków komercyjnych")
tsdisplay(zawod_ts, main="Współczynnik aktywności zawodowej")

# c) dekompozycje na podstawie różnych modeli regresji #########################

#Dekompozycje
sumagot_ts1 <- decompose(sumagot_ts) 
plot(sumagot_ts1) 
plot(sumagot_ts1$seasonal) 
plot(sumagot_ts1$trend) 
plot(sumagot_ts1$random) 

zawod_ts1 <- decompose(zawod_ts) 
plot(zawod_ts1) 
plot(zawod_ts1$seasonal) 
plot(zawod_ts1$trend) 
plot(zawod_ts1$random) 

#Dekompozycja na podstawie modelu regresji

#time series suma gotowki w obiegu
sumagot_ts2 <- sumagot_ts
sumagot_ts3 <- tslm(sumagot_ts2~trend)
sumagot_ts4<-tslm(sumagot_ts2 ~ trend + season)

plot(sumagot_ts2)
lines(fitted(sumagot_ts3), col = "red", lty = 5, lwd = 2) 
lines(fitted(sumagot_ts4), col = "green", lty = 5, lwd = 2) 
sumagot_ts5 <- decompose(sumagot_ts2, type = "multiplicative")

#Wykresy sumy gotowki w obiegu
tsdisplay(sumagot_ts5$random) 
tsdisplay(residuals(sumagot_ts4)) 

#Ramka danych Współczynnik aktywności zawodowej
zawod_ts2 <- zawod_ts
zawod_ts3 <- tslm(zawod_ts2~trend)
zawod_ts4<-tslm(zawod_ts2 ~ trend + season)

plot(zawod_ts2)
lines(fitted(zawod_ts3), col = "red", lty = 5, lwd = 2) 
lines(fitted(zawod_ts4), col = "green", lty = 5, lwd = 2) 
zawod_ts5 <- decompose(zawod_ts2, type = "multiplicative")

#Wykresy Współczynnika aktywności zawodowej
tsdisplay(zawod_ts5$random) 
tsdisplay(residuals(zawod_ts4)) 

#Dekompozycja na podstawie modelu regresji w trendzie wielomianowym 
sumagot_ts6 <- tslm(sumagot_ts2 ~ poly(trend, raw=TRUE, degree = 2)) 
plot(sumagot_ts2) 
lines(fitted(sumagot_ts6), col = "magenta", lty = 5, lwd = 2) 

zawod_ts6 <- tslm(zawod_ts2 ~ poly(trend, raw=TRUE, degree = 5)) 
plot(zawod_ts2) 
lines(fitted(zawod_ts6), col = "red", lty = 5, lwd = 2) 

# d) Eliminacja trendu i sezonowości ###########################################

#odsezonowanie szeregu sumy gotowki w obiegu
sumagot_ts5 <- decompose(sumagot_ts2, type = "multiplicative")
sumagot_tsUS <- seasadj(sumagot_ts5)
plot(sumagot_ts2)
lines(sumagot_tsUS, col = "magenta", lty = 1) 

#odsezonowanie szeregu Współczynnika aktywności zawodowej
zawod_ts5 <- decompose(zawod_ts2, type = "multiplicative")
zawod_tsUS<-seasadj(zawod_ts5)
plot(zawod_ts2, lwd = 1)
lines(zawod_tsUS, col = "red", lty = 1, lwd = 1) 

# e) Uczynienie szeregów stacjonarnymi, ########################################
# sprawdzenie czy są one realizacją szumu białego

#Zamiana szeregu na stacjonarny - suma gotowki w obiegu
plot(sumagot_ts2)
tsdisplay(sumagot_ts2)
sumabox<-BoxCox(sumagot_ts2,lambda=0)
sumadiff<-diff(sumabox,lag=12)
tsdisplay(sumadiff)
sumadiff2<-diff(sumadiff,lag=1)
tsdisplay(sumadiff2)

#Realicja modelu bialego?
Acf(sumadiff2, lag.max=100)
Pacf(sumadiff2,lag.max=100)

#Zamiana szeregu na stacjonarny - współczynnik aktywności zawodowej
plot(zawod_ts2)
tsdisplay(zawod_ts2)
zawodbox<-BoxCox(zawod_ts2,lambda=0)
zawoddiff<-diff(zawodbox,lag=12)
tsdisplay(zawoddiff)
zawoddiff2<-diff(zawoddiff,lag=1)
tsdisplay(zawoddiff2)

#Realicja modelu bialego?
Acf(zawoddiff2, lag.max=100) 
Pacf(zawoddiff2,lag.max=100)

# f) Wyznaczenie współczynnika modelu AR #######################################

#Wyznaczenie współczynnika modelu AR
sumagot_tsAR <- sumagot_ts
plot(sumagot_tsAR, main = "Suma gotówki w obiegu") 
tsdisplay(sumagot_tsAR) 
lamb.auto <- BoxCox.lambda(sumagot_tsAR)
sumagot_tsAR1 <- BoxCox(sumagot_tsAR, lambda = lamb.auto) 
tsdisplay(sumagot_tsAR1) 
sumagot_tsARdiff <- diff(sumagot_tsAR1, lag = 12) 
tsdisplay(sumagot_tsARdiff) 
sumagot_tsARdiff2 <- diff(sumagot_tsARdiff, lag = 1) 
tsdisplay(sumagot_tsARdiff2) 

#Szukanie wartości p
Acf(sumagot_tsARdiff2, lag.max = 100)
Pacf(sumagot_tsARdiff2, lag.max = 100)

#Wyznaczanie wspolczynnika modelu autoregresji, p wybrane wczesniej
ywcheck <- ar(sumagot_tsARdiff2, aic = FALSE,order.max = 96, method = "yule-walker")
print(ywcheck)

mlecheck <- ar(sumagot_tsARdiff2, aic = FALSE,order.max = 50, method = "mle")
print(mlecheck)


#Wyznaczanie wspolczynnika modelu autoregresji, automatycznie dobierany rzad
ywcheck2 <- ar(sumagot_tsARdiff2, aic = TRUE, order.max = 100, method = "yule-walker")
print(ywcheck2)


#Wyznaczenie współczynnika modelu AR cd.
zawod_tsAR <- zawod_ts
plot(zawod_tsAR, main = "Współczynnik aktywności zawodowej") 
tsdisplay(zawod_tsAR) 
lamb.auto <- BoxCox.lambda(zawod_tsAR)
zawod_tsAR1 <- BoxCox(zawod_tsAR, lambda = lamb.auto) 
tsdisplay(zawod_tsAR1) 
zawod_tsARdiff <- diff(zawod_tsAR1, lag = 12) 
tsdisplay(zawod_tsARdiff) 
zawod_tsARdiff2 <- diff(zawod_tsARdiff, lag = 1) 
tsdisplay(zawod_tsARdiff2) 

#Szukanie wartości p
Acf(zawod_tsARdiff2, lag.max = 100)
Pacf (zawod_tsARdiff2, lag.max = 100)

#Wyznaczanie wspolczynnika modelu autoregresji, p wybrane wczesniej
ywcheck3 <- ar(zawod_tsARdiff2, aic = FALSE,order.max = 56, method = "yule-walker")
print(zawod_tsARdiff2)

mlecheck2 <- ar(zawod_tsARdiff2, aic = FALSE,order.max = 30, method = "mle")
print(mlecheck2)

#Wyznaczanie wspolczynnika modelu autoregresji, automatycznie dobierany rzad
ywcheck4 <- ar(zawod_tsARdiff2, aic = TRUE, order.max = 100, method = "yule-walker")
print(zawod_tsARdiff2)


# G) wyznaczenie współczynników dla modelu MA(q) ############################### 

#MODEL AR I SEZONOWOSC PDPKT g

sumagot_arima <- Arima(sumagot_tsARdiff2, order = c(5,0,0)) 
summary(sumagot_arima)

zawod_arima <- Arima(zawod_tsARdiff2, order = c(30,0,0)) 
summary(zawod_arima)

# H) Wyznaczenie optymalnych modeli z wykorzystaniem funkcji ###################
# auto.arima() oraz wyznaczenie ich współczynników

(sumagot_aicc <- auto.arima(sumagot_ts2, ic = c("aicc")))
(sumagot_aic <- auto.arima(sumagot_ts2, ic = c("aic")))
(sumagot_bic <- auto.arima(sumagot_ts2, ic = c("bic")))

(zawod_aicc <- auto.arima(zawod_ts2, ic = c("aicc")))
(zawod_aic <- auto.arima(zawod_ts2, ic = c("aic")))
(zawod_bic <- auto.arima(zawod_ts2, ic = c("bic")))

# i) Porównanie analizowanych modeli, wybór najlepszego ########################

summary(sumagot_aicc)
summary(sumagot_aic)
summary(sumagot_bic)

summary(zawod_aicc)
summary(zawod_aic)
summary(zawod_bic)

# j) Prognozowanie z wykorzystaniem metod naiwnych, ############################
# dobór najlepszej metody dla danego szeregu.

#Metoda sredniej
sumagot_f <- sumagot_ts
sumagot_forecast <- meanf(sumagot_f, h = 36) 
plot(sumagot_forecast, main = "Prognoza sumy gotówki na podstawie sredniej") 

zawod_f <- zawod_ts
zawod_forecast <- meanf(zawod_f, h = 36) 
plot(zawod_forecast, main = "Prognoza Wsp. Akt. Zawodowej na podstawie sredniej") 

#Metoda naiwna i naiwna sezonowa
sumagot_naive <- naive(sumagot_f, h = 36) 
plot(sumagot_naive,main= "Prognoza sumy gotówki na podstawie metody naiwnej") 

sumagot_snaive <- snaive(sumagot_f, h = 36) 
plot(sumagot_snaive, main = "Prognoza sumy gotówki na podstawie metody naiwnej sezonowej") 

zawod_naive <- naive(zawod_f, h = 36) 
plot(zawod_naive,main= "Prognoza Wsp. Akt. Zawodowej na podstawie metody naiwnej") 

zawod_snaive <- snaive(zawod_f, h = 36) 
plot(zawod_snaive, main = "Prognoza Wsp. Akt. Zawodowej na podstawie metody naiwnej sezonowej") 

#Metoda uwzględniająca dryf
sumagot_dryf <- rwf(sumagot_f, h = 36, drift = TRUE) 
plot(sumagot_dryf, main = "Prognoza sumy gotówki na podstawie metody uwzgl. dryf")

zawod_dryf <- rwf(zawod_f, h = 36, drift = TRUE) 
plot(zawod_dryf, main = "Prognoza Wsp. Akt. Zawodowej na podstawie metody uwzgl. dryf")

#dobór najlepszej metody prognozy
(accuracy(sumagot_forecast))
(accuracy(sumagot_naive))
(accuracy(sumagot_snaive))
(accuracy(sumagot_dryf))

(accuracy(zawod_forecast))
(accuracy(zawod_naive))
(accuracy(zawod_snaive))
(accuracy(zawod_dryf))

################################################################################