if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
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

library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(stats)
library(dplyr)


# 4.2. Análise de Desempenho de Métodos Heurísticos na resolução do problema de Escalonamento

# # Para avaliar a eficiência das MH é usual considerarmos o tempo de processamento 
# na obtenção da solução (CPU time). Quanto menor o tempo de execução de um método 
# mais eficiente será a técnica de otimização. O ficheiro (Cpu.run.time.csv) contém 
# o tempo de processamento (em segundos) das três MH (MH1, MH2 e MH3) na resolução 
# das 50 instâncias com menor dimensão.

# ==================================== Ex2 ====================================

# Import data from mspandata.csv file
cpu_runtime <-
  read.csv(file = "Data/Cpu.run.time.secs.csv")
View(cpu_runtime)

# Numero total de linhas do ficheiro (50)
cpu_file_size <- length(cpu_runtime$X); cpu_file_size
summary(cpu_runtime)

# ================================= alínea a) =================================

# Construir um boxplot que contenha os tempos de processamento de cada MH na resolução de cada instância.


# === Boxplot com cada MH ===
boxplot(cpu_runtime[3:5],col = 'blue', horizontal = TRUE, main="Tempo de processamento", ylab="Metodos", xlab="Segundos")

# === CUN ===
boxplot(cpu_runtime[3], main="Tempo de processamento CUN", ylab="Segundos")

# === SPT ===
boxplot(cpu_runtime[4], main="Tempo de processamento SPT", ylab="Segundos")

# === LPT ===
boxplot(cpu_runtime[5], main="Tempo de processamento LPT", ylab="Segundos")

# ================================= alínea b) =================================

# Verificar se existem diferenças significativas nos tempos médios de processamento entre as três técnicas.


#Tratamento dos dados
# === CUN ===
cpu_CUN<- data.frame(Problema = cpu_runtime$Problema,
                     Metodo = c(rep("CUN", 50)),
                     Tempo = cpu_runtime$CUN)
View(cpu_CUN)

# === LPT ===
cpu_LPT <- data.frame( Problema = cpu_runtime$Problema,
                       Metodo = c(rep("LPT", 50)),
                       Tempo = cpu_runtime$LPT)
View(cpu_LPT)

# === SPT ===
cpu_SPT <- data.frame( Problema = cpu_runtime$Problema,
                       Metodo = c(rep("SPT", 50)),
                       Tempo = cpu_runtime$SPT)
View(cpu_SPT)

runtime_all_in_one <- data.frame(
                      Problema = c(cpu_LPT$Problema, cpu_CUN$Problema, cpu_SPT$Problema),
                      Metodo = c(cpu_LPT$Metodo, cpu_CUN$Metodo, cpu_SPT$Metodo),
                      Tempo= c(cpu_LPT$Tempo, cpu_CUN$Tempo, cpu_SPT$Tempo))
View(runtime_all_in_one)
summary(runtime_all_in_one)

# H0: M1 = M2 = M3; A distribuição dos k tratamentos é a mesma
# H1: M1 != M2 != M3; A distribuição dos k tratamentos é diferente


# Teste de Friedman
# Tratar para deixar em cada linha o metodo, o tempo e o problema, emparelhados

friedman.test(y = runtime_all_in_one$Tempo, groups = runtime_all_in_one$Metodo, blocks = runtime_all_in_one$Problema)


#p-value = 2.2e-16 muito inferior a 0.05, logo rejeita-se a hipotese nula e confirma-se a diferença
# significativa, logo, deve-se recorrer ao post hoc para identificar o mais eficiente.


PMCMRplus::frdAllPairsConoverTest(y = runtime_all_in_one$Tempo, groups = runtime_all_in_one$Metodo, blocks = runtime_all_in_one$Problema)

# ================================= alínea c) =================================

# Se sim, identificar qual a MH mais eficiente.


# ================================= alínea d) =================================

# Determinar a matriz de correlação entre os tempos de processamento de cada MH e interprete os resultados.
# Verificar os presupostos
# 1 se existem outliers significativos 
# É identificado outliers significativos como pode-se observar no boxplot do execício a.
# Com os seguintes histogramas pode-se observar a presença de valores que saem do padrão e também, a não normalidade dos.
# === CUN ===
hist(cpu_runtime$CUN)

# === SPT ===
hist(cpu_runtime$SPT)

# === LPT ===
hist(cpu_runtime$LPT)

matrix <- as.matrix(cpu_runtime[3:5]) ; matrix

# Como trata-se de amostras grandes, o teste de kendal será descartado.
problemas_cor_spearman <- cor(matrix, method = c("spearman"))
problemas_cor_spearman


corrplot(problemas_cor_spearman, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
