print("a")

1:1:100
1:10

# calc de amplitude, n mt usada.
dados=c(0.9,0.8,0.3,1.1,1.2,1.3,0.7,0.5,3.4,2.6)
max(dados)-min(dados)

diff(range(dados))

dados=c(0.9,0.8,0.3,1.1,1.2,1.3,0.7,0.5,-0.1,0,-0.7)
# cálculo da variância
var(dados)

# cálculo do desvio padrão
sd(dados)


library(e1071)

set.seed(33)
dados <- rnorm(100,mean=15,sd=5)
# momento centrado de ordem 3
moment(dados, order=3, center=TRUE)

set.seed(55)
dados <- rchisq(55,0.9,df=5)
skewness(dados)












amostra <- c(rep('L',4),rep('C',23),rep('D',16),rep('O',7))
data <- factor(amostra ,c('L','C','D','O'),
               labels=c("Leitura", "Cinema","Desporto", "Outros"))
tabela <- table(data); tabela


tabela.rel <- prop.table(tabela); tabela.rel





barplot(tabela)


library(ggplot2)
dados <- data.frame(ocup=c(rep('Leitura',4),rep('Cinema',23),
                           rep('Desporto',16),rep('Outros',7)))
dfd <- as.data.frame(table(dados))
ggplot(data=dfd,aes(x=dados,y=Freq,fill=dados)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(label=Freq),vjust=1.6,color="red",size=5.5) +
  scale_fill_brewer(palette="Blues") +
  theme(axis.title.x = element_text(size=16,color='black',face='bold'),
        axis.title.y = element_text(size=16,color='black',face='bold'),
        axis.text.x = element_text(size=14,color='black',face='bold'),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold") )



pie(tabela)




dados1=rchisq(100,.2, df=7)
dados2=rchisq(100,5, df=7)
classes=c("dados1","dados2")
boxplot(dados1,dados2,names=classes,col=c(4,2))






dados <- rnorm(1000,10,2)
hist(dados) # cut(), breaks().







