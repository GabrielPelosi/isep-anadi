#TP 2


#1) 
#a)
#t � val estatistifca teste da amostra
# df -> graus de liberdade
#p-value: valor muito alto, n�o rejeito a hipotese nula
#� poss�vel calcular esses val separadamento, mas n�o repcisamos de saber isso.
#One Sample t-test -> faz o teste a uma amostra simple.

rendas <- c(980, 650, 950, 800, 980, 500, 750, 680, 650, 795, 600, 900, 700, 700, 600,
650, 650, 650, 1000, 800)
mu0 <- 720

t.test(rendas, mu=mu0, alternativa="two.sided")

#b) a conclusao � que como o p-value � superior a 0.05(valor de alfa), n se rejeita a hipotese nula e pornante 
# nivel de significancia � superior a 5%, n�o h� evid estat�stica que o pre�o
# diferente de 720�


#2)
#Saber se as rendas antigas s�o menos com que as atuais, saber se a m�dia � mais cara ou n�o, assim
#vamos concluir a competitividade.
#Como no enunciado ... tem de se fazer um teste unilateral � direita com hipotese: H0: u= 720 vc u>720


rendas_a <- c(1600, 880, 650, 800, 500, 900, 700, 1040, 599, 650, 700, 850, 1100, 500, 800,
            990, 1390, 1590, 700, 1100) # errado, valores errados, corrigir
mu0 <-720
t.test(rendas_a, mu=mu0, alternativa="greater")



#3)

dieta <- c(
  550, 690, 750, 690, 800, 980, 980, 1300, 750, 475 ,680, 700, 650, 640, 795,
  650, 600, 1050, 950, 780
  
)

mu0 <- 700

t.test(dieta, mu=mu0, alternativa="two.sided")



















