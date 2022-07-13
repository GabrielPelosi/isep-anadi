

##TP6/7

#verificar se as variaveis são númericas(1.65...) ou são labels(classe, nomes, categorias: "alto","amarelo","gordo", não precisa de ser binário: "alto" ou "baixo").

#Com isso, verifica-se se será usada regressão ou classificação.
#Regressão para números e classificação para labels.

#Clasificação com árvores também.

#são modelos de classificação

#Não se mistura dados de treino com dados de testes
#Os dados são separados aleatóriamente no modelo 30/70.

#Os testes nas regrssões são contas de erro

#Na classificação 

#Contar as instnacias, se era pra dar amarelo e deu azul está mal
#Faz-se as contagens e assim obtem os resultados dos testes.

library(rpart)
library(mlbench)

data(BreastCancer)

dim(BreastCancer)

head(BreastCancer)

#Treinar o modelo com esses dados para classificar se o cancer é beligno ou maligno.


summary(BreastCancer)

#cor e boxplot
library(corrplot)

#Dados fortemente correlacionados que são dependentes um do outro tem de ser pré-processados, por ex: coluna salarios e coluna 3x salarios
#São formente correlacionadas pois uma depende da outr, ent uma tem  de ser excluida
#valores outliers e NA também tem de ser pré-processados e eliminados para não estragar o modelo.


#!! uma árvore com 2 ramos é mal sinal
#Pode advir de dados dependentes e outliers ou nas.

BreastCancer$Cl.thickness <- as.numeric(BreastCancer$Cl.thickness)
BreastCancer$Cell.size <- as.numeric(BreastCancer$Cell.size)
BreastCancer$Cell.shape <- as.numeric(BreastCancer$Cell.shape)
BreastCancer$Marg.adhesion <- as.numeric(BreastCancer$Marg.adhesion)
BreastCancer$Epith.c.size <- as.numeric(BreastCancer$Epith.c.size)
BreastCancer$Bare.nuclei <- as.numeric(BreastCancer$Bare.nuclei)
BreastCancer$Bl.cromatin <- as.numeric(BreastCancer$Bl.cromatin)
BreastCancer$Normal.nucleoli <- as.numeric(BreastCancer$Normal.nucleoli)
BreastCancer$Mitoses <- as.numeric(BreastCancer$Mitoses)

BreastCancer$Id <- NULL
BreastCancer = na.omit(BreastCancer)

#create an object of the features
bc= cor(BreastCancer[ ,1:9])
corrplot.mixed(bc)



library(reshape2)
library(ggplot2)

BreastCancer.m = melt(BreastCancer, id.var = "Class")
View(BreastCancer.m)
ggplot(data=BreastCancer.m,aes(x=Class, y=value))+ 
  geom_boxplot() + 
  facet_wrap(~variable, ncol = 3)



#####Tratamento dos dados #####
#Holdout

#Dividir em dados de teste e dados de teste em 30 e 70%. mas pode ser outros valores, 80 para treini e 20 para teste isso depende.

#aplica-se a grandes conjuntos de dados.

#para dados mais pequenos é o outro modelo


#Cross validation

#

library(rpart)

amostra <- sample(1:nrow(BreastCancer), 0.7*nrow(BreastCancer))

dados.modelo <- BreastCancer[amostra, ]
dados.teste <- BreastCancer[-amostra, ]

arvore <- rpart(Class ~., data = dados.modelo, method='class')

par(xpd = TRUE)
plot(arvore, compress = TRUE)
text(arvore, use.n = TRUE)



library(rpart.plot)
rpart.plot(arvore)



prevs.modelo <- predict(arvore, dados.teste, type='class')

head(prevs.modelo)


#num modelo binario

#positivo -> é o que queremos saber
#negativo -> o que não queremos saber

#Verdadeiro positivo -> quando o modelo diz que é positivo e é realemnte positivo
#Falso positivo -> uando o modelo diz que é positivo e não é positivo

#Verdadeiro negativo -> uando o modelo diz que é falso e é realemnte falso
#Falso negativo -> uando o modelo diz que é falso e não é falso


#accuracy -> (vp + vn) / (p + n)
#taxa de erro -> 1- accuracy
#precision -> vp/ (vp + fp)
#Recall -> Vp / (VP + VN)

#F1 = 2 x Precision x Recall / (Precisio+ Recall)


#Matriz de confusão
print("matriz de confusão")
m.conf <- table(dados.teste$Class, prevs.modelo)
print(m.conf)


#accuracy = VP+VN/n=(m.conf[1,1,]+m.conf[2,2])/n

taxa.acerto <- 100*round((m.conf[1,1]+m.conf[2,2])/sum(m.conf),4)

cat(paste("taxa de acerto: ", taxa.acerto, "%"))


accuracy = numeric()
recall <- numeric()
precision <- numeric()

for (i in 1:10){
  amostra<- sample(1:nrow(BreastCancer), 0.7*nrow(BreastCancer))
  dados.treino <- BreastCancer[amostra, ]
  dados.teste <- BreastCancer[ -amostra, ]
  
  arvore <- rpart(Class ~., data = dados.treino, method='class')
  prevs.modelo <- predict(arvore, dados.teste, type='class')
  
  m.conf <- table(dados.teste$Class, prevs.modelo)
  
  accuracy[i] = (m.conf[1,1]+m.conf[2,2])/sum(m.conf)
  recall[i] = m.conf[1,1]/(m.conf[1,1] + m.conf[1,2])
  
  precision[i] = m.conf[1,1] / (m.conf[1,1]+m.conf[2,1])
}

cat(paste("taxa de acerto media: ", 100*round(mean(accuracy),4), "%, desvio:", round(sd(accuracy),4)))


cat(paste("recall: ", round(mean(recall),3), "%, desvio:", round(sd(recall),3)))


cat(paste("precision: ", round(mean(precision),3), "%, desvio:", round(sd(precision),3)))


f1 = (2*round(mean(precision),3)*round(sd(recall),3)) / (round(mean(precision),3) + round(sd(recall),3))


cat(paste("\n F1: ", f1))











