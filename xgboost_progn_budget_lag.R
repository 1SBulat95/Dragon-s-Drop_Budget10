#Модель экстремального градиентного бустинга с отложенным воздействием результирующей и экзогенной переменной

#загрузка пакетов
library('e1071')
install.packages("mice")
library(mice)
library(readxl)
install.packages("openxlsx")
library("openxlsx")
install.packages("randomForest")
library(randomForest)
install.packages("xgboost")
library(xgboost)
library(Matrix)
install.packages("Metrics")
library(Metrics)

#загрузка данных
inc <- read.xlsx("C:/inc_lag.xlsx")
exp <- read.xlsx("C:/exp_lag.xlsx")

#восстановление пропущенных значений
md.pattern(inc)
miceMod <- mice(inc, method="rf")  
inc <- complete(miceMod)  
anyNA(inc)
md.pattern(inc)

md.pattern(exp)
miceMod <- mice(exp, method="rf")  
exp <- complete(miceMod)  
anyNA(exp)
md.pattern(exp)

#разделение выборки
inc1 <- inc$inc[1:10]
inc_l1 <- inc$inc_l[1:10]
trend1 <- inc$trend[1:10]
train_inc <- data.frame(inc1, inc_l1, trend1)

inc2 <- inc$inc[11:12]
inc_l2 <- inc$inc_l[11:12]
trend2 <- inc$trend[11:12]
test_inc <- data.frame(inc2, inc_l2, trend2)

exp1 <- exp$exp[1:10]
exp_l1 <- exp$exp_l[1:10]
trend3 <- exp$trend[1:10]
train_exp <- data.frame(exp1, exp_l1, trend3)

exp2 <- exp$exp[11:12]
exp_l2 <- exp$exp_l[11:12]
trend4 <- exp$trend[11:12]
test_exp <- data.frame(exp2, exp_l2, trend4)

#xgboost с регрессорами
sparse_matrix <- sparse.model.matrix(train_inc$inc1 ~ inc_l1 + trend1 + exp1 + exp_l1, data = train_inc)[,-1]
X_train_dmat = xgb.DMatrix(sparse_matrix, label = train_inc$inc1)

xgboost_model <- xgboost(data = X_train_dmat, label = train_inc$inc1, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "reg:squarederror")

#прогноз
sparse_matrix1 <- sparse.model.matrix(test_inc$inc2 ~ inc_l2 + trend2 + exp2 + exp_l2, data = test_inc)[,-1]
X_test_dmat = xgb.DMatrix(sparse_matrix1, label = test_inc$inc2)

colnames(X_test_dmat) <- NULL

pred <- predict(xgboost_model, X_test_dmat)

#оценка прогноза
pred
test_inc$inc2
mape(test_inc$inc2, pred)



