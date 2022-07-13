# ===================== 4.1. Regressão =====================
library(readr)
library(fastDummies)

# Ex1 
# Carregar ficheiro "Clientes_DataSet.csv"
data_df <- read_csv(file = "Clientes_DataSet.csv", locale = locale(encoding = "latin1"))
View(data_df)

# Verificar dimensão
dim(data_df)

# Sumário
summary(data_df)


# Ex2 - Pré-processamento dos dados
# ==== a) Identificação de NA e limpeza do dataSet ====
sum(is.na(data_df))
data_df = na.omit(data_df)
sum(is.na(data_df))


# ==== b) Identifique dados inconsistentes e outliers  ====
library(reshape2)
library(ggplot2)

meltData <- melt(data_set)
boxplot(data=meltData, value~variable)

p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")


# ==== c) Implemente a seleção de atributos, se aplicável ====

# Criar data frame sem coluna ClienteID
data_set<-data_df[2:21]

# Converter dados para numérico
data_set$Fidelização <- as.numeric(data_set$Fidelização)
data_set$TarifaMensal <- as.numeric(data_set$TarifaMensal)
data_set$TotalTarifas <- as.numeric(data_set$TotalTarifas)
data_set$Colaborador <- ifelse(data_set$Colaborador=='Sim',1,0)
data_set$Dependentes <- ifelse(data_set$Dependentes=='Sim',1,0)
data_set$TipoServiço <- ifelse(data_set$TipoServiço =='Sim',1,0)
data_set$FaturaEletronica <- ifelse(data_set$FaturaEletronica=='Sim',1,0)
data_set$Genero <- ifelse(data_set$Genero=='Masculino',1,0)

# Criacao de colunas dummy para variaveis categoricas
data_set <-
  fastDummies::dummy_cols(
    data_set,
    select_columns = c(
      "LinhasMultiplas",
      "ServiçoInternet",
      "SegurançaOnline",
      "CópiadeSegurançaOnline",
      "ProteçãoTM",
      "SuporteTécnico",
      "ServiçoStreamingTV",
      "ServiçoStreamingFilmes",
      "TipodeContrato",
      "MétododePagamento"
    ),
    remove_selected_columns = TRUE
  )
View(data_set)

# Data frame a ser usado apenas na Regressao com dados todos numericos
reg_data<-data_set[1:41]
reg_data$Ativo <- ifelse(data_set$Ativo=='Sim',1,0)
# Substituir espacos nos nomes das colunas por "_"
names(reg_data) <- gsub(" ", "_", names(reg_data)) 
View(reg_data)

# Criar dados treino e teste
index <- sample (1:nrow(reg_data), as.integer(0.7 * nrow(reg_data)))
dados.modelo <- reg_data[index, ] # dados treino (70%)
dados.teste <- reg_data[-index,] # dados teste (30%)


# Ex3 - Crie um diagrama de correlação entre todos os atributos
library(corrplot) 

# Corrplot com metade dos dados, mas suficiente para retirar as conclusoes necessarias
corrplot(round(cor(reg_data[,1:20]), digits=3), tl.cex = 0.75)

# Corrplot completo sem os nomes de colunas (para visualização)
corrplot(cor(reg_data[,1:41]), tl.pos='n')


# Ex4 - Modelo de regressão linear simples para a variável objetivo para
# determinar o período de “Fidelização” usando a tarifa mensal (“TarifaMensal”) 

# ==== a) Função linear resultante  ====
# lm([target variable] ~ [predictor variables], data = [data source])
slr.model <- lm(Fidelização ~ TarifaMensal, data = dados.modelo)
slr.model

# ==== b) Visualizar reta correspondente ao modelo de RLS e diagrama de dispersão ====
plot(dados.modelo$TarifaMensal, dados.modelo$Fidelização, 
     pch = 20, cex = 0.5,
     xlab = "TarifaMensal", ylab = "Fidelização", main = "Reta modelo de regressão linear simples")
abline(slr.model$coefficients[1], slr.model$coefficients[2], col="red")

# ==== c) Erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) sobre os 30% casos de teste ====
summary(slr.model)

slr.pred <- predict(slr.model, dados.teste)
slr.d <- dados.teste$Fidelização - slr.pred # diferença entre os valores aproximados e reais

# Mean absolute error - MAE
slr.mae<- mean(abs(slr.d)); slr.mae

# Root Mean Squared - RMSE
slr.rmse <- sqrt(mean(slr.d ^ 2)); slr.rmse


# Ex5 - Prever o TotalTarifas 

# TotalTarifas = Fidelização * TarifaMensal

# ==== a) Regressão linear múltipla  ====

# lm([target variable] ~ ., data = [data source])
mlr.model = lm(TotalTarifas ~ ., data = dados.modelo) 
summary(mlr.model) # informação do modelo e qualidade do ajuste
summary(mlr.model)$coefficient

# Retirar variaveis nao significativas
mlr.model = lm(TotalTarifas ~ Fidelização + TarifaMensal, data = dados.modelo) 
mlr.model

# ==== b) Árvore de regressão (rpart) ====
library(rpart)
library(rpart.plot)

rpart.model <-rpart(TotalTarifas ~ ., method = "anova", data = dados.modelo)
rpart.model
rpart.plot(rpart.model, digits = 3, fallen.leaves = TRUE, type = 3, extra = 101)


# ==== c) Rede neuronal (neuralnet) ====

# Normalização min-max
minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data.norm <- as.data.frame(lapply(reg_data, minmaxnorm))

# Holdout
set.seed(123)

nn.index <- sample(1:nrow(data.norm), 0.7 * nrow(data.norm))
nn.data.train <- data.norm[nn.index,]
nn.data.tst <- data.norm[-nn.index,]

summary(nn.data.train$TotalTarifas)
summary(nn.data.tst$TotalTarifas)

library(neuralnet)
set.seed(739)

# Rede c/ 1 nó no nível interno
numnodes<-1
nn.model <- neuralnet(TotalTarifas ~ ., data = nn.data.train, hidden = numnodes)
plot(nn.model)

# Rede c/ 3 nós no nível interno
numnodes <- 3
nn.model_3 <- neuralnet(TotalTarifas ~ ., data = nn.data.train, hidden = numnodes, stepmax=1e7)
plot(nn.model_3)

# Rede c/ 2 níveis internos: 6, 2 nós
numnodes<-c(6,2)
nn.model_6to2 <- neuralnet(TotalTarifas ~ ., data = nn.data.train, hidden = numnodes, stepmax=1e7)
plot(nn.model_6to2)


# Ex6 - Compare os resultados obtidos  pelos modelos referidos na questão 5, usando o erro
# médio absoluto (MAE) e raiz quadrada do erro médio (RMSE). 

# ==== Regressão linear múltipla ====
mlr.pred <- predict(mlr.model, dados.teste)
mlr.pred

mlr.d <- dados.teste$TotalTarifas - mlr.pred

# MAE
mlr.mae <- mean(abs(mlr.d)); mlr.mae
# RMSE
mlr.rmse <- sqrt(mean(mlr.d^2)); mlr.rmse


# ==== Árvore de regressão ====
rpart.pred <- predict(rpart.model, dados.teste)
arv.d <- rpart.pred - dados.teste$TotalTarifas

# MAE
arv.mae<-mean(abs(arv.d)); arv.mae
# RMSE
arv.rmse <- sqrt(mean((arv.d) ^ 2)); arv.rmse

# ==== Rede neuronal ====
# change troubleshooting column names
colnames(nn.data.tst)[38] <- "MétododePagamento_Cartão_de_Crédito_.automatico."
colnames(nn.data.tst)[41] <- "MétododePagamento_Transferência_Bancária_.automatico."

# Desnormalizar os dados para avaliar o modelo 
minmaxdesnorm <- function(x, goal.attrib) {
  return (x * (max(goal.attrib) - min(goal.attrib)) + min(goal.attrib))
}
MAE <- function(test, predicted) {
  mean(abs(test - predicted))
}
RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

# == 1 node ==
nn.pred <- compute(nn.model, nn.data.tst[,-9])
nn.pred.totaltarifas <- minmaxdesnorm(nn.pred$net.result, reg_data$TotalTarifas) 
test.totaltarifas <- minmaxdesnorm(nn.data.tst$TotalTarifas, reg_data$TotalTarifas)

nn.mae <- MAE(test.totaltarifas, nn.pred.totaltarifas); nn.mae
nn.rmse <- RMSE(test.totaltarifas, nn.pred.totaltarifas); nn.rmse

# == 3 nodes ==
nn.pred_3 <- compute(nn.model_3, nn.data.tst)
nn.pred_3.totaltarifas <- minmaxdesnorm(nn.pred_3$net.result, reg_data$TotalTarifas)
test.totaltarifas <- minmaxdesnorm(nn.data.tst$TotalTarifas, reg_data$TotalTarifas)

nn.mae_3 <- MAE(test.totaltarifas, nn.pred_3.totaltarifas); nn.mae_3
nn.rmse_3 <- RMSE(test.totaltarifas, nn.pred_3.totaltarifas); nn.rmse_3

# == 6 to 2 nodes ==
nn.pred_6to2 <- compute(nn.model_6to2, nn.data.tst)
nn.pred_6to2.totaltarifas <- minmaxdesnorm(nn.pred_6to2$net.result, reg_data$TotalTarifas)
test.totaltarifas <- minmaxdesnorm(nn.data.tst$TotalTarifas, reg_data$TotalTarifas)

nn.mae_6to2 <- MAE(test.totaltarifas, nn.pred_6to2.totaltarifas); nn.mae_6to2
nn.rmse_6to2 <- RMSE(test.totaltarifas, nn.pred_6to2.totaltarifas); nn.rmse_6to2


# Comparacao entre os modelos 
data.frame(Modelo=c("Regressao linear simples", "Regressao multipla", "Arvore Regressao", "Redes neuronais (1)",
                    "Redes neuronais (3)", "Redes neuronais (6->2)"), 
           MAE=c(slr.mae,mlr.mae,arv.mae, nn.mae, nn.mae_3, nn.mae_6to2),
           RMSE=c(slr.rmse,mlr.rmse, arv.rmse, nn.rmse, nn.rmse_3, nn.rmse_6to2))


# Ex7 - Justifique se os resultados obtidos para os dois melhores modelos são
# estatisticamente significativos

# H0: Os resultados obtidos para os dois melhores modelos são estatisticamente significativos.
# H1: Os resultados obtidos para os dois melhores modelos não são estatisticamente significativos.
# a = 0.05

arv.sample<-c(arv.mae, arv.rmse) # amostra arvore de regressao
nn.sample<-c(nn.mae, nn.rmse) # amostra rede neuronal

res<-t.test(arv.sample, nn.sample); res 
# p-value = 0.09826 (> 0.05, logo nao se rejeita H0)


# ===================== 4.2. Classificação =====================

# Ex8 - Estudar a capacidade preditiva relativamente ao atributo Ativo

data_df <- read_csv(file = "Clientes_DataSet.csv", locale = locale(encoding = "latin1"))

# Convert data to numeric
data_set$Fidelização <- as.numeric(data_set$Fidelização)
data_set$TarifaMensal <- as.numeric(data_set$TarifaMensal)
data_set$TotalTarifas <- as.numeric(data_set$TotalTarifas)
data_set$TotalTarifas <- NULL

amostra.modelo <- sample (1:nrow(data_set), as.integer(0.7 * nrow(data_set)))
dados.modelo <- data_set[amostra.modelo, ] # dados treino (70%)
dados.teste <- data_set[-amostra.modelo,] # dados teste (30%)

# ==== a) Árvore de decisão ====
library(rpart)
library(rpart.plot)

# Arvore
class_tree.model <- rpart(Ativo ~., data = dados.modelo, method='class')
rpart.plot(class_tree.model, digits = 3, fallen.leaves = TRUE, type = 3, extra = 101)

class_tree.pred <- predict(class_tree.model, dados.teste, type='class')

head(class_tree.pred)

# Matriz de Confusao
matrix.conf <- table(dados.teste$Ativo, class_tree.pred)
matrix.conf

# Taxa acerto
taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
sprintf("Taxa de acerto: %g%%", taxa.acerto)


# ==== c) K-vizinhos-mais-próximos ====
library(class)
library(neuralnet)

#Função para normalizar
minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_set_knn <-
  fastDummies::dummy_cols(
    data_set,
    select_columns = c(
      "LinhasMultiplas",
      "ServiçoInternet",
      "SegurançaOnline",
      "CópiadeSegurançaOnline",
      "ProteçãoTM",
      "SuporteTécnico",
      "ServiçoStreamingTV",
      "ServiçoStreamingFilmes",
      "TipodeContrato",
      "MétododePagamento"
    ),
    remove_selected_columns = TRUE
  )

data_set_knn_ativo <- data_set_knn$Ativo
data_set_knn$Ativo <- NULL

data_set_knn.norm <- as.data.frame(lapply(data_set_knn, minmaxnorm))


data_set_knn.norm$Ativo <- data_set_knn_ativo

data_set_knn.norm$TotalTarifas <- NULL

# ==== b) Rede neuronal ====
set.seed(123)


# 1 node
amostra.modelo <- sample (1:nrow(data_set_knn.norm), as.integer(0.7 * nrow(data_set_knn.norm)))
dados.modelo <- data_set_knn.norm[amostra.modelo,] # dados treino (70%)
dados.teste <- data_set_knn.norm[-amostra.modelo,] # dados teste (30%)

nn.model <- neuralnet(Ativo ~ ., data = dados.modelo,  hidden = 1,
                      linear.output = FALSE, stepmax = 1e7, threshold=0.04)
plot(nn.model)

nn.pred <- compute(nn.model, dados.teste)

results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred$net.result)

results$actual <- ifelse(results$actual =='Sim',1,0)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

matrix.conf <- table(actual,prediction.1)

taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
sprintf("Taxa de acerto: %g%%", taxa.acerto)

  
  # 4 nodes
  nn.model <- neuralnet(Ativo ~ ., data = dados.modelo,  hidden = 4,
                        linear.output = FALSE, stepmax = 1e7, threshold=0.04)
  plot(nn.model)
  
  nn.pred <- compute(nn.model, dados.teste)
  
  results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred$net.result)
  
  results$actual <- ifelse(results$actual =='Sim',1,0)
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdf=data.frame(roundedresults)
  attach(roundedresultsdf)
  
  matrix.conf <- table(actual,prediction.1)
  
  taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
  sprintf("Taxa de acerto: %g%%", taxa.acerto)

# 4 nodes
nn.model <- neuralnet(Ativo ~ ., data = dados.modelo,  hidden = 4,
                      linear.output = FALSE, stepmax = 1e7, threshold=0.04)
plot(nn.model)

nn.pred <- compute(nn.model, dados.teste)

results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred$net.result)

results$actual <- ifelse(results$actual =='Sim',1,0)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

matrix.conf <- table(actual,prediction.1)

taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
sprintf("Taxa de acerto: %g%%", taxa.acerto)



# Rede 2 nívels 6 nodes

numnodes<-c(6,2)

nn.model <- neuralnet(Ativo ~ ., data = dados.modelo,  hidden = numnodes, linear.output = FALSE, stepmax = 1e7, threshold=0.04)
plot(nn.model)

names(nn.model)

nn.pred <- compute(nn.model, dados.teste)

results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred$net.result)

results$actual <- ifelse(results$actual =='Sim',1,0)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

matrix.conf <- table(actual,prediction.1)

taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
sprintf("Taxa de acerto: %g%%", taxa.acerto)


# Rede 3 níveis e 10 nós
numnodes<-c(10,3)


nn.model <- neuralnet(Ativo ~ ., data = dados.modelo,  hidden = numnodes, linear.output = FALSE, stepmax = 1e7, threshold=0.04)

plot(nn.model)

names(nn.model)

nn.pred <- compute(nn.model, dados.teste)

results <- data.frame(actual = dados.teste$Ativo, prediction = nn.pred$net.result)

results$actual <- ifelse(results$actual =='Sim',1,0)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

matrix.conf <- table(actual,prediction.1)

taxa.acerto <-100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
sprintf("Taxa de acerto: %g%%", taxa.acerto)


set.seed(123)
amostra.modelo <- sample (1:nrow(data_set_knn.norm), as.integer(0.7 * nrow(data_set_knn.norm)))
dados.modelo <- data_set_knn.norm[amostra.modelo, -40] # dados treino (70%)
dados.teste <- data_set_knn.norm[-amostra.modelo, -40] # dados teste (30%)
train.ativo <- data_set_knn.norm[amostra,40]
tst.ativo <- data_set_knn.norm[-amostra, 40]


k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  knn.pred <- knn(train=dados.modelo, test=dados.teste, cl= train.ativo, k=i)
  m.conf <- table(tst.ativo,knn.pred)
  accuracy <- c(accuracy, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i)
}
cat("k max: ", k[which.max(accuracy)])
## k max: 31
## accuracy:  0.7270142

plot(k, accuracy)


# Ex9 - Obter a média e o desvio padrão da taxa de acerto da previsão do atributo 
# Ativo com os dois melhores modelos obtidos na alínea anterior

# Método k-fold cross validation

kf <- 10
folds <- sample(1:kf, nrow(data_set_knn), replace = TRUE)
cv.error <- matrix(nrow = kf, ncol = 2)

k<-31

for (i in 1:kf){
  train.cv <- data_set_knn.norm[folds != i,]
  test.cv <- data_set_knn.norm[folds == i,]
  train.atv <- data_set_knn.norm[folds != i, 40]
  tst.atv <- data_set_knn.norm[folds == i, 40]
  knn.pred <- knn(train=train.cv[, -40], test=test.cv[, -40], cl= train.atv, k=35)
  m.conf <- table(tst.atv,knn.pred)
  rpart.model <- rpart(Ativo ~. , method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type="class")
  m.conf2 <- table(tst.atv,rpart.pred)
  cv.error[i, ] <- c(sum(diag(m.conf))/sum(m.conf),
                     sum(diag(m.conf2))/sum(m.conf2))
}

cv.error

apply(cv.error,2,mean)

apply(cv.error,2,sd)


# Ex10 - Verifique se existe diferença significativa no desempenho dos dois melhores
# modelos obtidos anteriormente

# H0: O desempenho é igual nos dois melhores modelos.
# H1: O desempenho é diferente nos dois melhores modelos.
# a = 0.05

knn_rpart_pred <- data.frame(model = rep(c("KNN", "RPART"), each = 10),
                             accuracy = c(cv.error[,1],  cv.error[,2]))
knn_rpart_pred$accuracy <- as.numeric(knn_rpart_pred$accuracy)
View(knn_rpart_pred)

library("ggpubr")
ggboxplot(knn_rpart_pred, x = "model", y = "accuracy", 
          color = "model", palette = c("#00AFBB", "#E7B800"),
          ylab = "accuracy", xlab = "Models")


#Os dados são independentes? Sim
#Os dados dos 2 grupos seguem uma destribuição normal?
#Será realizado o teste shapiro para verificar a normalidade.

#H0: Os dados seguem uma distribuição normal
#H1: Os dados não seguem uma distribuição normal

# Shapiro-Wilk normality test for KNN's accuracy
with(knn_rpart_pred, shapiro.test(accuracy[model == "KNN"]))
# Shapiro-Wilk normality test for RPART's weights
with(knn_rpart_pred, shapiro.test(accuracy[model == "RPART"]))

#Como os p-values obtidos são superiores ao nível de significancia 0.05, pode-se confirmar a normalidade
#dos dados


res.ftest <- var.test(accuracy ~ model, data = knn_rpart_pred)
res.ftest
#Como o p-value foi superior a 0.05, pode-se confirmar a hipotese nula e concluir que não
#há diferença significativa entre o desempenho do knn e da árvore de regressão.


# Ex11 - Compare os resultados dos modelos. Discuta em detalhe qual o modelo que apresentou
# melhor e pior desempenho de acordo com os critérios: Accuracy; Sensitivity; Specificity e F1. 

#Recall = Sensitivity
accuracy_knn <- c()
recall_knn <- c()
precision_knn <- c()
specificity_knn <- c()
for (i in 1:10){
  amostra.modelo <- sample (1:nrow(data_set_knn.norm), as.integer(0.7 * nrow(data_set_knn.norm)))
  dados.modelo <- data_set_knn.norm[amostra.modelo, -40] # dados treino (70%)
  dados.teste <- data_set_knn.norm[-amostra.modelo, -40] # dados teste (30%)
  train.ativo <- data_set_knn.norm[amostra,40]
  tst.ativo <- data_set_knn.norm[-amostra, 40]
  
  knn.pred <- knn(train=dados.modelo, dados.teste, cl= train.ativo, k=31)
  
  m.conf <- table(tst.ativo,knn.pred)
  
  accuracy_knn[i] <- (m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf)
  recall_knn[i] = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  
  precision_knn[i] = m.conf[1,1] / (m.conf[1,1]+m.conf[2,1])
  specificity_knn[i] =  m.conf[2, 2] / ( m.conf[2, 2] + m.conf[2,1])
  
}

sprintf("Taxa de Acerto Media = %g%%, Desvio = %g", 100 * round(mean(accuracy_knn), 4), round(sd(accuracy_knn), 4))
sprintf("Reacall = %g%%, Desvio = %g", round(mean(recall_knn),3), round(sd(recall_knn),3))
sprintf("Precision = %g%%, Desvio = %g", round(mean(precision_knn),3), round(sd(precision_knn),3))
sprintf("Specificity = %g%%, Desvio = %g", round(mean(specificity_knn),3), round(sd(specificity_knn),3))

f1 = (2 * round(mean(precision), 3) * round(sd(recall), 3)) / (round(mean(precision), 3) + round(sd(recall), 3))
sprintf("F1 = %g", f1)

accuracy <- c()
recall<- c()
precision<- c()
specificity<- c()
for (i in 1:10){
  amostra <- sample(1:nrow(data_set), 0.7 * nrow(data_set))
  dados.treino <- data_set[amostra,]
  dados.teste <- data_set[-amostra,]
  
  arvore <- rpart(Ativo ~ ., data = dados.treino, method = 'class')
  prevs.modelo <- predict(arvore, dados.teste, type = 'class')
  
  m.conf <- table(dados.teste$Ativo, prevs.modelo)
  
  accuracy[i] = (m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf)
  recall[i] = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  
  precision[i] = m.conf[1,1] / (m.conf[1,1]+m.conf[2,1])
  
  specificity[i] =  m.conf[2, 2] / ( m.conf[2, 2] + m.conf[2,1])
}

sprintf("Taxa de Acerto Media = %g%%, Desvio = %g", 100 * round(mean(accuracy), 4), round(sd(accuracy), 4))
sprintf("Reacall = %g%%, Desvio = %g", round(mean(recall),3), round(sd(recall),3))
sprintf("Precision = %g%%, Desvio = %g", round(mean(precision),3), round(sd(precision),3))
sprintf("Specificity = %g%%, Desvio = %g", round(mean(specificity),3), round(sd(specificity),3))

f1 = (2 * round(mean(precision), 3) * round(sd(recall), 3)) / (round(mean(precision), 3) + round(sd(recall), 3))
sprintf("F1 = %g", f1)

