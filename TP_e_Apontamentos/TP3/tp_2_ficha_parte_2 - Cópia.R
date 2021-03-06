

#TP3

#5)
#H0: Ua = Ub = Uc = Ud vs H1: existe uma media diferente das outras.
#

# antes de fazer o teste, fazer um boxplot dos dados.
#Um boxplot para a empresa A e um para empresa B

# primeir, definir as amostras todas com o mesm comprimento, adicionei NA ao fim

empresa_a <- c(1.0, 0.8, 1.9, 1.1, 2.7, NA)
empresa_b <- c(1.7, 2.5, 3.0, 2.2, 3.7, 1.9)
empresa_c <- c(1.0, 1.3, 3.2, 1.4, 1.3, 2.0)
empresa_d <- c(3.8, 2.8, 1.9, 3.0, 2.5, NA)


# boxplot das empresas
dados <- as.data.frame(cbind(empresa_a, empresa_b, empresa_c, empresa_d))

View(dados)
boxplot(dados)
##################
#explicaco de como fazer plot

x <- 0:0.1:7
y <- cos(x)
plot(x,y)

plot(y~x)
##################

# em um data frame, se h� duas colunas, uma AV e outra Filmes.
# para fazer um ANOVA, deve-se fazer AV~Filmes.

##############################

#Manualmente  construir um obj desses

Empresa <- c(rep('empresa_a',6),rep('empresa_b',6),rep('empresa_c',6),rep('empresa_d',6))
Custo <- c(empresa_a, empresa_b, empresa_c, empresa_d)
Mdados1 <- data.frame(Empresa,Custo)
View(Mdados1)

# modo mais t�cnico e simples. em caso de dados fossem enormes, era melhr fazer assim

library(reshape2)
Mdados2 <- melt(dados, variable.name = "Empresa", value.name = "Custo")
View(Mdados2)

#para fazer o teste anova
# A parte num�rica sempre depende da parte categ�rica
#" ou seja, o custo depedne da empresa"

mod1 = lm(Custo ~ Empresa, data = Mdados2)
anova(mod1)

# no enum, o alfa � 0.05, ent�o eu rejeito a hip�tese nula, pois o p value � menor que alfa
# ou seja h� um agravamento significante com o fator empresa pois o valor de significancia...

my_aov <- aov(Custo ~ Empresa, data = Mdados2)
summary(my_aov)
 



# Testes n�o param�tricos

#TP3
#1)

# os 45% n�o � uma amostra, � uma popula��o em geral.

# Po = 0.45
# n=25
# sucesso = 13
# Ho = P=Po vs H1: P > Po
#binom.test(n� success, tamanho da pop referente ao sucesso, Po=(ou seja,represetacaoda outra populacao, civis no caso, em 45% que pontuaram mais de 50 pontos), alternativa)

my_test <- binom.test(13,25,p=0.45,alternative="greater")
my_test$p.value
#p.value = 0.3, � muito superior a 0.05 (alfa), 
# n�o se rejeita a hipotese nula.





#2) identificaar o caso de sucesso, que � a crian�a escolher o balde vermelho
# a probabilidade de sucesso � 0.5 pois ... ou escolhe-se o balde azul ou o vermelho, ent�o h� 50% de chance de sucesso e de falha.

my_test <- binom.test(15,19,p=0.5, alternative="greater")
my_test$p.value
# o p-value � abaixo de 0.05 ent rejeitamos a hipotese nula, 
#   ent�o realmente as crian�asa preferem os baldes vermelhos.



#Quando o n � mt grande, usa-se o prop.test
# se for pequeno, usa-se o binom.test

# 3) prop.test -> p-value = 6.0...
    # binom.test -> p-value = 8.0...

#4)

library(readxl)
Ex4 <- read.csv("C:/Users/ggpel/OneDrive/Documentos/Isep/ANADI/Ex4.csv", sep=";")
View(Ex4)


tamanho_amostra <- length(Ex4$produto); tamanho_amostra

n_sucessos <- sum(Ex4$produto == 'A'); n_sucessos


my_t <- prop.test(n_sucessos,tamanho_amostra, alternative = "two.sided")
my_t$p.value

my_t_b <- binom.test(n_sucessos,tamanho_amostra, alternative = "two.sided")
my_t_b$p.value


# 5)




























