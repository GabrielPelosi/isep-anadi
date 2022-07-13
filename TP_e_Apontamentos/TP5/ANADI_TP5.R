#TP 5

#======= a) =======
library(datarium)
data(marketing)


#======= b) =======
summary(marketing)

#======= c) =======
#Deve-se separar os dados de treino e de teste aleatoriamente, para não viciar o modelo.
#Segui-se o modelo Holdout, em que 70% são usados para treino e 30% para testes.

set.seed(123)
index <- sample(1:nrow(marketing), as.integer(0.7 * nrow(marketing)))

market.train <- marketing[index, ]
market.tst <- marketing[-index, ]

market.train
market.tst

summary(market.train$sales)

summary(market.tst$sales)

#======= d) =======

#---i)---
slr.model <- lm(sales~youtube, data=market.train)
slr.model

summary(slr.model)

#---ii)---
#Linha de regressão
#Usa-se os dados de treino do modelo para as bolinhas e a reta para os valores coeficientes do modelo treinado.
plot(market.train$youtube, market.train$sales, pch=20)

abline(slr.model$coefficients[1], slr.model$coefficients[2], col='red')

#---iii)---
#predict para testar o modelo

slr.pred <- predict(slr.model, market.tst)

#depois de testes, buscar o d para determinar os erros médios

d_slr <- slr.pred - market.tst$sales

#Mae - Mean Absolute Error
slr_mae <- mean(abs(d))

#RMSE - Root Mean Squared Error
slr_rmse <- sqrt(mean(d^2))


#======= e) =======
#Para representar todos mete uma bolinha, ao invés do +.
mlr.model <- lm(sales ~ youtube + facebook + newspaper, data = market.train)
mlr.model
summary(mlr.model)

##======= f) =======
#Simplificar seria remover o newspaper pois ao analisar o sumario idenfica-se que há muita diference entre os outros
#No youtube, facebook aparecem até asteristicos, no newspaper não.
mlr.model_1 <- lm(sales ~ youtube + facebook, data = market.train)
mlr.model_1
summary(mlr.model_1)


mlr.pred_1 <- predict(mlr.model_1, market.tst)
d_mlr_pred_1 <- mlr.pred_1 - market.tst$sales

#Mae - Mean Absolute Error
mlr_1_mae <- mean(abs(d_mlr_pred_1)) ; slr_mae

#RMSE - Root Mean Squared Error
mlr_1_rmse <- sqrt(mean(d_mlr_pred_1^2)) ;mlr_1_rmse

#======= g) =======
library(rpart)
library(rpart.plot)

#anova -> regressão
#class -> classificação

rpart.model <- rpart(sales ~ .,method="anova", data=market.train)
summary(rpart.model)

#======= h) =======
rpart.plot(rpart.model, digits=3)

#======= i) =======
#predict para testar o modelo
rpart.pred <- predict(rpart.model, market.tst)

#depois de testes, buscar o d para determinar os erros médios
d <- rpart.pred - market.tst$sales

rpart_m <- mean(abs(d))


rpart_rmse <- sqrt(mean(d^2))


#===========================
#Exercício 2
#===========================


#======= a) =======
data(swiss)
#======= b) =======
View(swiss)
#======= c) =======



