#KNN

library(readr)
library(reshape2)
library(ggplot2)

#Carregar o dataset

abalone <- read_csv(file = 'abalone.data')
colnames(abalone) <- c('sex', 'length', 'diameter', 'height', 'whole_weight','shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings')


abalone <- data.frame(abalone)

View(abalone)


str(abalone)


head(abalone)


summary(abalone)


#Correlação
library(corrplot)
corrplot(round(cor(abalone[,2:9]),digits=3))

#plot
ggplot(abalone) + aes(abalone$rings, color = abalone$sex) + geom_density()

#Histograma
hist(abalone$rings, main = "Abalone Rings")


#Função para normalizar
minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#Converter do sexo em numérico, 0 para Femea, 1 para macho e 2 para Indeterminado

abalone$sex <- (ifelse(abalone$sex == 'F',0,abalone$sex))

abalone$sex <-(ifelse(abalone$sex == 'M',1,abalone$sex))

abalone$sex <-(ifelse(abalone$sex == 'I',2,abalone$sex))

abalone$sex <- as.numeric(abalone$sex)

table(abalone$Sex)
abalone.norm <- as.data.frame(lapply(abalone, minmaxnorm))

View(abalone.norm)

#"Adicionar o atributo age apartir da media do tamanho dos aneis"
abalone$age<-as.factor(ifelse(abalone$rings<mean(abalone$rings),"Young","Adult"))
table(abalone$age)


#Retirar falso previsor, pois idade vem do valor do anel, logo são fortemente correalcionadas

abalone <- abalone[,-9]
abalone.norm <- abalone.norm[,-9]
abalone.norm$age <- abalone$age


head(abalone.norm)


# Holdout
set.seed(123)
index <-sample(1:nrow(abalone.norm), 0.7*nrow(abalone.norm))
ncols <- dim(abalone.norm)[2]
data.train <- abalone.norm[index,-9] #Matriz (ou data frame) de exemplos de treino
data.tst <- abalone.norm[-index,-9] #Matriz (ou data frame) de exemplos de teste
train.rings <- abalone.norm[index,9] #Vetor com as respostas de cada observação do conjunto de treino
tst.rings <- abalone.norm[-index, 9]
train.rings


library(class) #Classifcação: knn()
set.seed(123)
k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  knn.pred <- knn(train=data.train, test=data.tst, cl= train.rings, k=i)
  m.conf <- table(tst.rings,knn.pred)
  accuracy <- c(accuracy, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i)
}
cat("k max: ", k[which.max(accuracy)])
## k max: 27


#USABO APENAS EM CASO DE REGRESSAO
k <- c()
rmse <- c()
# nn.pred.ringsDesnormalizar os dados para avaliar o modelo
minmaxdesnorm <- function(x,goal.attrib) {
  x*(max(goal.attrib)-min(goal.attrib))+min(goal.attrib)
}
RMSE <- function(test, predicted){
  sqrt(mean((test - predicted)^2))
}


#comparação com arvore de classificacao

library(rpart)
kf <- 10
folds <- sample(1:kf, nrow(abalone), replace = TRUE)
cv.error <- matrix(nrow = kf, ncol = 2)
## k max (alinea anterior): 27
k<-27
for (i in 1:kf){
  train.cv <- abalone.norm[folds != i,]
  test.cv <- abalone.norm[folds == i,]
  train.rings <- abalone[folds != i, 9]
  tst.rings <- abalone[folds == i, 9]
  knn.pred <- knn(train=train.cv[,-9], test=test.cv[,-9], cl= train.rings, k=27)
  m.conf <- table(tst.rings,knn.pred)
  rpart.model <- rpart(age ~. , method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type="class")
  m.conf2 <- table(tst.rings,rpart.pred)
  cv.error[i, ] <- c(sum(diag(m.conf))/sum(m.conf),
                     sum(diag(m.conf2))/sum(m.conf2))
}

cv.error

apply(cv.error,2,mean)

apply(cv.error,2,sd)

