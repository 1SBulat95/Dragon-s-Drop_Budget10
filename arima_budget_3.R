#предобработка

#загрузка пакетов
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library("openxlsx")
install.packages("forecast")
library(forecast)
install.packages("Metrics")
library(Metrics)
install.packages("tseries")
library("tseries")

#загрузка данных
inc <- read.xlsx("C:/inc.xlsx")
inc
inc1 <- ts(inc, frequency = 1, start = "2010")
inc11 <- inc$inc[1:10]
inc11
inc22 <- inc$inc[11:12]
inc22
inc11 <- ts(inc11, frequency = 1, start = "2010")
inc11
inc22 <- ts(inc22, frequency = 1, start = "2020")
inc22
exp <- read.xlsx("C:/exp.xlsx")
exp11 <- exp$exp[1:10]
exp11
exp22 <- exp$exp[11:12]
exp22

#логарифмирование
inc11 <- log(inc11)
inc11
inc22 <- log(inc22)
inc22

#модель с регрессорами
mod_1 <- auto.arima(inc11, stepwise = FALSE, ic = "aicc", xreg = exp11, allowdrift = FALSE)
mod_1
#прогнозирование
forecast = predict(mod_1, newxreg = exp22)

#оценка прогноза
forecast$pred
inc22
mape(inc22, forecast$pred)
