#TP7

#k-folds cross-validation

nrFolds <- 10
accuracy <- numeric()
recall <- numeric()
precision <- numeric()

folds <- rep_len(1:nrFolds, nrow(BreastCancer))
folds <- sample(folds, length(folds))

for (k in 1:nrFolds) {
  
  fold <- wich(folds == k)
  dados.treino <- BreastCancer[fold, ]
  dados.teste <- BreastCancer[-fold,]
  
  arvore <- rpart(Class ~., data = dados.treino, method = "class")
  
  prevs.modelo <- predict(arvore, dados.teste, type = "class")
  
  head(prevs.modelo)
  
  m.conf <- table(dados.teste$Class, prevs.modelo)
  
  accuracy[k] <- ((m.conf[1,1] + m.conf[2,2]) / sum(m.conf)) 
  recall[k] <- (m.conf[1,1] / (m.conf[1,1] + m.conf[1,2])) 
  precision[k] <- (m.conf[1,1] / (m.conf[1,1] + m.conf[2,1])) 
  
}



numnode <- c(6,3)

nn.model <- neuralnet(Class ~., data = data.train, hidden=numnodes, linear.output= FALSE)



####3



#neural

plot(nn.model)

nn.pred <- precit(nn.model, data.tst[, -10])

table(data.tst$class, applly(nn.pred, 1, wich.max))

#table(data.tst$Class, nn.pred[,1]> 0.5) # , nn.pred[,1 ] > 0.5

#ou
#nn.pred <- compute(nn.model, data.tst[, -10])
#m.conf2 <- table(data.tst$class, round(nn.pred$net.result[,1], digits=0))
#
3



