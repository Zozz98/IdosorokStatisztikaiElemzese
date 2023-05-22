# install.packages('quantmod') ha nincs feltelepitve
library(astsa)
library(quantmod)
library(tseries)
library(fGarch)

#Adatbegyujtes(4iG zaroar es volumen)
fourig = getSymbols('4IG.BD')
fourig_close = `4IG.BD`$`4IG.BD.Close`
fourig_volume = `4IG.BD`$`4IG.BD.Volume`

#Kiplotolni mindkettot
plot(fourig_close, main="4iG Close Prices")
plot(fourig_volume, main="4iG Volume")

#Ketteszedni az adatsort, egy 2018 elotti es egy 2018 utanira
after_2018=window(fourig_close, start='2018-01-01', end = NULL)
plot(after_2018, main="4iG Close Price after 2018")
before_2018 = window(fourig_close, start=NULL, end='2018-01-01')
plot(before_2018, main="4iG Close Price before 2018")

#Stacionarizalni a 2018 utani adatsort (logaritmikus hozam)
logret_after_2018 = na.omit(diff(log(after_2018)))
plot(logret_after_2018, main='Daily Log Return after 2018')

#Stacionarizalni a 2018 elotti adatsort (logaritmikus hozam)
logret_before_2018 = na.omit(diff(log(before_2018)))
plot(logret_before_2018, main='Daily Log Return before 2018')

#Stacionarizalni a teljes adatsort
fourig_log_return = na.omit(diff(log(fourig_close)))
plot(fourig_log_return, main="Daily Log Return")

#Kiszamolni az elmult evekre vetitett evenkenti atlagos hozamot es szorast
fourig_annual_return = mean(fourig_log_return) * 252
fourig_annual_volatility = sd(fourig_log_return) * sqrt(252)

#Autokorrelacios fuggveny 2018 elottre, utanra, egeszre
#ACF -> MA meghatarozasa
#PACF -> AR meghatarozasa
acf2(logret_after_2018, main="ACF & PACF after 2018")
acf2(logret_before_2018, main="ACF & PACF before 2018")
acf2(fourig_log_return, main="ACF & PACF from 2007-today")

#Histogram es QQplot
hist(fourig_log_return)
qqnorm(fourig_log_return)

#ARMA(3,1) 2018 utani idosor
sarima_model_after_2018 = sarima(logret_after_2018, 3,0,1, fixed = c(NA,0,NA,NA), no.constant = T)
sarima_model_after_2018$ttable

#ARMA(2,1) 2018 elotti idosor
sarima_model_before_2018 = sarima(logret_before_2018, 3,0,1, fixed=c(NA,NA,0,NA),  no.constant = T)
sarima_model_before_2018$ttable

#Box-test 2018 utanra
Box.test(sarima_model_after_2018$fit$residuals, lag=20, type = 'Ljung-Box', fitdf = 3)
#fitdf=ahany db parameter van a modellben -> 3+1, de 1 ki lett nullazva
#Ljung-box test: p-value = 0.1506 -> elfogadjuk, hogy korrelalatlanok a rezidualisok

#Box-test 2018 elottre
Box.test(sarima_model_before_2018$fit$residuals, lag=20, type = "Ljung-Box", fitdf=3)

#Normalitas:
ks.test(logret_after_2018)
#2.2*10^-16 -> Nem kovet normalis eloszlast

#Ket idoszakra GARCH(x,y), kiiratni tablazatba a parametereket, 
#omega alpha beta, hogyan viszonyulnak egymashoz

#GARCH Modellezes lepesei:
#-ARIMA[x]
#-ARCH hatas: Ljung-Box-test (r_t)^2-re[x]
#-Rendek meghatarozasa (r_t)^2 ACF&PACF alapjan[x]
#-Modellillesztes: Parameterek ML-becslese[x]
#-Rezidualisok elemzese: Elfogadhato egy modell, ha a rezidualis negyzetek korrelalatlanok[x]

#ARCH/GARCH 2018 utan
acf(sarima_model_after_2018$fit$residuals^2, main="ACF residual^2 after 2018")
Box.test(logret_after_2018^2, lag=20, type='Ljung-Box', fitdf=3)
# Rezidualisok negyzete nem korrelelatlan, tehat van ARCH hatas
garch_fit_after_2018 = garchFit(~garch(1,1), data=sarima_model_after_2018$fit$residuals)
summary(garch_fit_after_2018)

#ARCH/GARCH 2018 elott
acf(sarima_model_before_2018$fit$residuals^2, main="ACF residual^2 before 2018")
garch_fit_before_2018 = garchFit(~garch(1,1), data=sarima_model_before_2018$fit$residuals)
summary(garch_fit_before_2018)
#Elfogadjuk a parameterbecsleseket, es a Ljung-Box alapjan korrelalatlanok a rezidualisok es a negyzetei is
#Ez egy jo modell

#Volatilitasklaster
plot(garch_fit_before_2018)
#Selection: 2,3

plot(garch_fit_after_2018)
