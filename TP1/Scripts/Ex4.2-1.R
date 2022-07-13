# 4.2. Análise de Desempenho de Métodos Heurísticos na resolução do problema de Escalonamento
if(!require(multcompView)){install.packages("multcompView")}
if(!require(PMCMR)){install.packages("PMCMR")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(PMCMRplus)){install.packages("PMCMRplus")}
if(!require(NSM3)){install.packages("NSM3")}

library(NSM3)
library(PMCMR)
library(rcompanion)
library(multcompView)
library(PMCMRplus)
# Note-se que, a resolução exata fornece o plano com o makespan mínimo 
# (quanto menor for o makespan mais eficaz é a solução). Geralmente, recorre-se 
# para resolver instâncias de grande dimensão a técnicas inspiradas na 
# Inteligência Artificial – Meta-Heurísticas (MH), que resolvem um problema de 
# escalonamento de forma satisfatória, mas que, geralmente, não garantem a 
# solução ótima.

# ==================================== Ex1 ====================================

# Import data from mspandata.csv file
makespan_data <-
  read.csv(file = "Data/mspandata.csv")
View(makespan_data)

# Numero total de linhas do ficheiro (320)
ms_file_size <- length(makespan_data$Mspan); ms_file_size
summary(makespan_data)

# ================================= alínea a) =================================

# eixo das abcissas (xlab): problemas 
# eixo das ordenadas (ylab): valor do makespan das três MH's para cada instância
library(ggplot2)
library(dplyr)

ggplot(data = makespan_data, 
       aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))


# ================================= alínea b) =================================

# Considerar apenas os dados relativos às 10 instâncias com menor tamanho. 
# Comentar sobre a existencia de diferenças significativas entre o desempenho das três técnicas?

# retirar apenas as primeiras 10 instancias 
smallest_10_data<-filter(makespan_data, makespan_data$problema >= 1 & makespan_data$problema <= 10) 
View(smallest_10_data)

smallest_10<-ggplot(data = smallest_10_data, 
                    aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))

# De acordo com o gráfico, existem diferencas significativas. O CUN possui o melhor desempenho, aproximando-se do OPT. 
# As restantes tecnicas, SPT e LPT, divergem do CUN e OPT, ou seja, o seu mspan é bastante elevado o que as torna pouco eficaz.

  # H0: O desempenho é igual entre as tres tecnicas.
  # H1: O desempenho é diferente entre as tres tecnicas.

# Teste de Friedman
#Remover OPT
smallest_10_data <- subset(smallest_10_data, Metodo!="OPT")
View(smallest_10_data)

friedman.test(y = smallest_10_data$Mspan, groups = smallest_10_data$Metodo, blocks = smallest_10_data$problema)
friedman.test(Mspan ~ Metodo | problema, data = smallest_10_data)

# p-value = 0.0003707. Como o p-value é menor que α rejeita-se H0.

#Test post-hoc

#Namenyi test

PMCMRplus::frdAllPairsConoverTest(y = smallest_10_data$Mspan, groups = smallest_10_data$Metodo, blocks = smallest_10_data$problema)

# ================================= alínea d) =================================

# 1. Considerar apenas os dados relativos às 20 instâncias com maior tamanho. 

biggest_20_data<-filter(makespan_data, makespan_data$problema > 60 & makespan_data$problema <= 80) 
View(biggest_20_data)

biggest_20<-ggplot(data = biggest_20_data, 
                   aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))


# 2. Comentar sobre a existencia de diferenças significativas entre o desempenho das três técnicas?

  # H0: O desempenho é igual entre as tres tecnicas.
  # H1: O desempenho é diferente entre as tres tecnicas.

#Teste de Friedman
#remover OPT
biggest_20_data <- subset(biggest_20_data, Metodo!="OPT")
#biggest_20_data$problema <- c(rep(1:20,3))
View(biggest_20_data)

friedman.test(y = biggest_20_data$Mspan, groups = biggest_20_data$Metodo, blocks = biggest_20_data$problema)
friedman.test(Mspan~Metodo|problema, data=biggest_20_data)

# p-value = 5.878e-13. Como o p-value é menor que α rejeita-se H0.

#Test post-hoc Friedman

#Namenyi test
#posthoc.friedman.nemenyi.test(y = biggest_20_data$Mspan, groups = biggest_20_data$Metodo, blocks = biggest_20_data$problema)

PMCMRplus::frdAllPairsConoverTest(y = biggest_20_data$Mspan, groups = biggest_20_data$Metodo, blocks = biggest_20_data$problema)

# ================================= alínea e) =================================

# makespan normalizado (Cnorm)
# CMH - makespan de uma MH
# COPT = makespan ótimo
# Cnorm = (CMH – COPT)/COPT

# ===== LPT ===== 
makespan_LPT_Norm <- filter(makespan_data, makespan_data$Metodo=='LPT')

for (i in 1:nrow(makespan_LPT_Norm)) {
  # for-loop over rows
  makespan_LPT_Norm[i] <-
    ((makespan_LPT_Norm[i] - makespan_OPT[i]) / makespan_OPT[i])
}
View(makespan_LPT_Norm)

# ===== CUN ===== 
makespan_CUN_Norm <- filter(makespan_data, makespan_data$Metodo=='CUN')

for(i in 1:nrow(makespan_CUN_Norm)) {       # for-loop over rows
  makespan_CUN_Norm[i] <- ((makespan_CUN_Norm[i] - makespan_OPT[i]) / makespan_OPT[i])
}
View(makespan_CUN_Norm)

# ===== SPT ===== 
makespan_SPT_Norm <- filter(makespan_data, makespan_data$Metodo=='SPT')

for(i in 1:nrow(makespan_SPT_Norm)) {       # for-loop over rows
  makespan_SPT_Norm[i] <- ((makespan_SPT_Norm[i] - makespan_OPT[i]) / makespan_OPT[i])
}
View(makespan_SPT_Norm)

# 1. Criar gráfico usando makespan normalizado

# eixo das abcissas (xlab): problemas 
# eixo das ordenadas (ylab): valor do makespan das três MH's para cada instância

makespan_data_norm<-data.frame(problema = rep(1:80, 3),
                        Mspan=c(makespan_LPT_Norm$Mspan,
                              makespan_CUN_Norm$Mspan, 
                              makespan_SPT_Norm$Mspan),
                        Metodo = rep(c("LPT", "CUN", "SPT"), each=80))
View(makespan_data_norm)

all_data_norm<-ggplot(data = makespan_data_norm, aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))

# 2. Considerar apenas os dados relativos às 10/20 instâncias com menor/maior tamanho. 

# ==== 10 instancias de menor tamanho ====
smallest_10_data_norm<-filter(makespan_data_norm, makespan_data_norm$problema >= 1 & makespan_data_norm$problema <= 10) 
View(smallest_10_data_norm)

smallest_10_norm<-ggplot(data = smallest_10_data_norm, aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))

# ==== 20 instancias de maior tamanho ====
biggest_20_data_norm<-filter(makespan_data_norm, makespan_data_norm$problema > 60 & makespan_data_norm$problema <= 80) 
View(biggest_20_data_norm)


biggest_20_norm<-ggplot(data = biggest_20_data_norm, aes(x = problema, y = Mspan)) + geom_line(aes(colour = Metodo))

# 3. Comentar sobre a existencia de diferenças significativas entre o desempenho das três técnicas?

  # H0: O desempenho é igual para as tres tecnicas.
  # H1: O desempenho é diferente para as tres tecnicas.

# ==== 10 instancias de menor tamanho ====

# Teste de Friedman
friedman.test(y = smallest_10_data_norm$Mspan, groups = smallest_10_data_norm$Metodo, blocks = smallest_10_data_norm$problema)
friedman.test(Mspan~Metodo|problema, data=smallest_10_data_norm)

# p-value = 0.0003707. Como o p-value é menor que α, rejeita-se H0.

#Post-hoc test
#Namenyi test

PMCMRplus::frdAllPairsConoverTest(y = smallest_10_data_norm$Mspan, groups = smallest_10_data_norm$Metodo, blocks = smallest_10_data_norm$problema)

# ==== 20 instancias de maior tamanho ====

  # H0: O desempenho é igual para as tres tecnicas.
  # H1: O desempenho é diferente para as tres tecnicas.


#Test de Friedman
friedman.test(y = biggest_20_data_norm$Mspan, groups = biggest_20_data_norm$Metodo, blocks = biggest_20_data_norm$problema)
friedman.test(Mspan~Metodo|problema, data=biggest_20_data_norm)

# p-value = 2.061e-09. Como o p-value é menor que α, rejeita-se H0.

#Post-hoc test
#Namenyi test

PMCMRplus::frdAllPairsConoverTest(y = biggest_20_data_norm$Mspan, groups = biggest_20_data_norm$Metodo, blocks = biggest_20_data_norm$problema)


# ================================= alínea g) =================================

# Determinar, para cada MH (tecnica), a reta de regressão linear
# Use os dados relativos às 70 instâncias de menor dimensão. 

# Extrair as primeiras 70 instancias
smallest_70_LPT_norm <- filter(makespan_LPT_Norm, makespan_LPT_Norm$problema >= 1 & makespan_LPT_Norm$problema <= 70)
smallest_70_CUN_norm <- filter(makespan_CUN_Norm, makespan_CUN_Norm$problema >= 1 & makespan_CUN_Norm$problema <= 70)
smallest_70_SPT_norm <- filter(makespan_SPT_Norm, makespan_SPT_Norm$problema >= 1 & makespan_SPT_Norm$problema <= 70)

# y = mx + b (y = variavel resposta, x = variavel preditora)
# variável preditora - tamanho do problema (x)
# variável resposta - makespan normalizado (y)

# ===== LPT ===== 

rl_LPT <- plot(
  smallest_70_LPT_norm$problema ,
  smallest_70_LPT_norm$Mspan ,
  pch = 19,
  cex = 0.5,
  xlab = "Tamanho do Problema",
  ylab = "Makespan Normalizado",
  main = "Regressao Linear da Tecnica LPT")
reg_LPT <- lm(Mspan ~ problema, data = smallest_70_LPT_norm)
abline (reg_LPT, col = "blue")

# ===== CUN ===== 

rl_CUN <- plot (
  smallest_70_CUN_norm$problema ,
  smallest_70_CUN_norm$Mspan ,
  pch = 19,
  cex = 0.5,
  xlab = "Tamanho do Problema",
  ylab = "Makespan Normalizado",
  main = "Regressao Linear da Tecnica CUN")
reg_CUN<-lm(Mspan ~ problema, data = smallest_70_CUN_norm)
abline (reg_CUN, col = "blue")


# ===== SPT ===== 

rl_SPT <- plot (
  smallest_70_SPT_norm$problema ,
  smallest_70_SPT_norm$Mspan ,
  pch = 19,
  cex = 0.5,
  xlab = "Tamanho do Problema",
  ylab = "Makespan Normalizado",
  main = "Regressao Linear da Tecnica SPT")
reg_SPT<-lm(Mspan ~ problema, data = smallest_70_SPT_norm)
abline (reg_SPT, col = "blue")


# ================================= alínea h) =================================

# Verificar se os pressupostos sobre os resíduos são verificados (normalidade, homocedasticidade e independência).

# (1) NORMALIDADE
# Os resíduos seguem uma distribuição normal com média zero.

  # Processo grafico
    # LPT
    qqnorm (residuals (reg_LPT) , ylab = "Residuos LPT", xlab = "Quantis teoricos")
    qqline (residuals (reg_LPT))
    
    # CUN
    qqnorm (residuals (reg_CUN) , ylab = "Residuos CUN", xlab = "Quantis teoricos")
    qqline (residuals (reg_CUN))
    
    # SPT
    qqnorm (residuals (reg_SPT) , ylab = "Residuos SPT", xlab = "Quantis teoricos")
    qqline (residuals (reg_SPT))

  # Teste de Shapiro 
    # LPT
    shapiro.test(residuals (reg_LPT))
    
    library(nortest)
    lillie.test(residuals (reg_LPT))
    
    #CUN
    shapiro.test(residuals (reg_CUN))
    lillie.test(residuals (reg_CUN))
    
    
    # SPT
    shapiro.test(residuals (reg_SPT))
    lillie.test(residuals (reg_SPT))


# (2) HOMOCEDASTICIDADE

  # H0: A condição de homocedasticidade é verificada.
  # H1: a condição de homocedasticidade não é verificada.

  # Metodo grafico
    # LPT
    # Plot residuos vs. Valores ajustados
    par(mfrow =c(2,1))
    plot(fitted(reg_LPT), residuals(reg_LPT), xlab ="Val. Ajust.LPT", ylab =" Res.", pch=16, col="blue")
    abline(h=0)

    # CUN
    # Plot residuos vs. Valores ajustados
    par(mfrow =c(2,1))
    plot(fitted(reg_CUN), residuals(reg_CUN), xlab ="Val. Ajust. CUN", ylab =" Res.", pch=16, col="blue")
    abline(h=0)
    
    # SPT
    # Plot residuos vs. Valores ajustados
    par(mfrow =c(2,1))
    plot(fitted(reg_SPT), residuals(reg_SPT), xlab ="Val. Ajust. SPT", ylab =" Res.", pch=16, col="blue")
    abline(h=0)

  # Dividir os dados em dois conjuntos e testar se a variância é igual.
    # LPT
    mx_LPT = median(smallest_70_LPT_norm$problema)
    var.test (residuals (reg_LPT)[smallest_70_LPT_norm$problema > mx_LPT], residuals (reg_LPT)[smallest_70_LPT_norm$problema < mx_LPT])
    
    # CUN
    mx_CUN = median(smallest_70_CUN_norm$problema)
    var.test (residuals (reg_CUN)[smallest_70_CUN_norm$problema > mx_CUN], residuals (reg_CUN)[smallest_70_CUN_norm$problema < mx_CUN])
    
    # SPT
    mx_SPT = median(smallest_70_SPT_norm$problema)
    var.test (residuals (reg_SPT)[smallest_70_SPT_norm$problema > mx_SPT], residuals (reg_SPT)[smallest_70_SPT_norm$problema < mx_SPT])

# (3) INDEPENDENCIA

  # H0: Os resíduos são independentes
  # H1: Os resíduos não são independentes

  # Teste de Durbin-Watson
library(carData)
library(car)

    # LPT
    durbinWatsonTest(reg_LPT)
    # p-value = 

    # CUN
    durbinWatsonTest(reg_CUN)
    # p-value = 
    
    # SPT
    durbinWatsonTest(reg_SPT)
    # p-value = 

# fazer varias vezes e realizar t.test()