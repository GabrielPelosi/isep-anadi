#TP1_ex.

#A)

library(readxl)
PORDATA_dias_sem_chuva <- read_excel("C:/Users/ggpel/Downloads/PORDATA_N.º-de-dias-sem-chuva.xlsx", 
                                              range = "A8:J69")
View(PORDATA_dias_sem_chuva)


#B)
colnames(PORDATA_dias_sem_chuva)[1] <- "Anos"

View(PORDATA_dias_sem_chuva)



# c) # box plot -> caixa de bigodes

names(PORDATA_dias_sem_chuva[2:10])

boxplot(PORDATA_dias_sem_chuva[2:10], main="Dias sem chuva", ylab="Número de dias")


c(1,2,3,4,5,6,7) > 2 # retirna um array logico para cada valor da exp.



PORDATA_dias_sem_chuva[PORDATA_dias_sem_chuva == 0] <- NA # onde estiver 0 dias de chuva irá substituir por NA

boxplot(PORDATA_dias_sem_chuva[2:10], main="Dias sem chuva", ylab="Número de dias")

boxplot(PORDATA_dias_sem_chuva[2:10], main="Dias sem chuva", ylab="Número de dias", outline = F)



summary(PORDATA_dias_sem_chuva[2:10])

quantis.C8 <- quantile(PORDATA_dias_sem_chuva$`Castelo Branco`)
quantis.C8

summary(PORDATA_dias_sem_chuva$`Castelo Branco`)


quantis.PT <- quantile(PORDATA_dias_sem_chuva, na.rm = =TRUE)
quantis.PT


tab.P<-table(cut(PORDATA_dias_sem_chuva$Porto, breaks = 9))
tab.P

tab.P<-table(cut(PORDATA_dias_sem_chuva$Porto, breaks = c(150,200,250,300)))
tab.P


nc<.nclass.Sturges(PORDATA_dias_sem_chuva$Porto)
tab.P<- table(cut(PORDATA_dias_sem_chuva$Porto, breaks = nc))
tab.P


h.P<-hist(PORDATA_dias_sem_chuva$Porto)

h.B<-hist(PORDATA_dias_sem_chuva$Porto, breaks = 100) #sem braks
h.B

dsc.P<-ts(PORDATA_dias_sem_chuva$Porto, start = 1960)
dsc.F<-ts(PORDATA_dias_sem_chuva$Faro, start = 1960)
ts.plot(dsc.P,dsc.F, lty=c(1:2), main="Dias sem chuva",ylab="Número de dias", legend("ddd"),legend=c("Porto"))






# 2)


















