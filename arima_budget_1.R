#Модель авторегрессии и скользящего среднего (без определения типа процесса) доходов консолидированного бюджета субъекта РФ 
# загрузка пакетов
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library("openxlsx")
install.packages("forecast")
library(forecast)
install.packages("Metrics")
library(Metrics)

#загрузка данных
inc <- read.xlsx("C:/inc.xlsx")
inc
inc1 <- ts(inc, frequency = 1, start = "2010")
inc1 <- ts(inc, frequency = 1, start = "2010")
inc11 <- inc$inc[1:11]
inc11
inc22 <- inc$inc[12:12]
inc22
inc11 <- ts(inc11, frequency = 1, start = "2010")
inc11
inc22 <- ts(inc22, frequency = 1, start = "2021")
inc22
plot.ts(inc11)

#модель
library(forecast)
fit.arima <- auto.arima(inc11)
fit.arima

# прогнозирование
forecast = predict(fit.arima)

# оценка прогноза
forecast$pred
inc22
mape(inc22, forecast$pred)
