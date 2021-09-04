#предварительный анализ

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

#Логарифмирование
inc11 <- log(inc11)
inc11
inc22 <- log(inc22)
inc22

#Тест Дики-Фуллера
install.packages("tseries")
library("tseries")
adf.test(inc11, alternative = c("stationary"))
b <- diff(inc11)
adf.test(b, alternative = c("stationary"))
b1 <- diff(b)
adf.test(b1, alternative = c("stationary"))
b1

#сохранение
install.packages("openxlsx")
library("openxlsx")
write.xlsx(b1, "C:/Пользователи/Булат/Рабочий стол/вторая разность стационарная.xlsx", sheetName = "Sheet1", col.names = TRUE, showNA = TRUE)

#модель
install.packages("forecast")
library("forecast")
#без дополнительных регрессоров
mod_2 <- auto.arima(inc11, stepwise = FALSE, ic = "aicc", allowdrift = FALSE)
mod_2

# прогнозирование
forecast = predict(mod_2, n.ahead = 2)

# оценка прогноза
forecast$pred

inc22
mape(inc22, forecast$pred)

