# 4.1. Análise do funcionamento dos servidores VPN do DEI

# Definições:
# a) Falha - embora a razão de uma sessão ter uma curta duração possa ter diversas origens, para o efeito
#   deste estudo considera-se que qualquer sessão com duração igual ou inferior a um minuto representa uma falha.
# b) Número de falhas simultâneas - Em cada minuto t0, define-se o número de
#   falhas simultâneas como sendo o número de falhas que inicializaram ou que finalizaram no minuto t0.

# c) Acesso - Um acesso é uma sessão com duração superior a um minuto.
# d) Número de acessos simultâneos - Em cada minuto t0, define-se o número de
#   acessos simultâneos como sendo o número de acessos que inicializaram antes do minuto t0 e terminaram depois do minuto t0.

# Ex2 - Apenas dados relativos ao mês de dezembro de 2017

library(dplyr)
dez2017 <-
  filter(vpn_sessions,
         vpn_sessions$Data >= "2017-12-01" &
           vpn_sessions$Data <= "2017-12-31")
file_size_dez2017 <-length(dez2017$Servidor); file_size_dez2017 # o ficheiro tem 7258 linhas
View(dez2017)


# a) Efetue um gráfico que nas abcissas represente o tempo, e nas ordenadas o número de falhas
# simultâneas e o número de acessos simultâneos.

# Extrair e ordenar os todos os tempos do mês de dezembro 2017 
all_minutes_in_dez <- unique(c(dez2017$`Início de Sessão`, dez2017$`Fim de Sessão`)) %>% sort()

# Criar data frame vazio de falhas e acessos simultaneos
df_falhas_acessos_simultaneos = data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_falhas_acessos_simultaneos) <- c('Minuto', 'Num_Falhas_Simultaneas', 'Num_Acessos_Simultaneos')

# Percorrer todos os minutos existentes
for(min in all_minutes_in_dez){
  
  # Calcular acessos simultaneos 
  nAcessos_simultaneos <-
    filter(dez2017, dez2017$`Tempo de Sessão` > 1,
           dez2017$`Início de Sessão` <= min & dez2017$`Fim de Sessão` >= min) %>% summarise(nAcessos = n())
  dez2017_acessos = sum(nAcessos_simultaneos$nAcessos)

  # Calcular falhas simultaneas 
  nFalhas_simultaneas <- 
    filter(dez2017, dez2017$`Tempo de Sessão` <= 1, 
           dez2017$`Início de Sessão` == min | dez2017$`Fim de Sessão` == min) %>% summarise(nFalhas = n());
  dez2017_falhas = sum(nFalhas_simultaneas$nFalhas)
  
  # Rbind por linhas do data frame vazio criado com a nova informacao percorrida
  df_falhas_acessos_simultaneos <- rbind(df_falhas_acessos_simultaneos,
      data.frame(Tempo = min,
                 Num_Falhas_Simultaneas = dez2017_falhas,
                 Num_Acessos_Simultaneos = dez2017_acessos))
}
View(df_falhas_acessos_simultaneos)

# Criar ts.plot para visualizar o numero de falhas e acessos em cada t0
library(forecast)
ts.plot(ts(df_falhas_acessos_simultaneos[2:3]),
  main = "Falhas/Acessos simultâneos em dezembro de 2017",
  ylab = "Nº falhas/acessos simultaneos",
  xlab = "Tempo",
  start = 00:00,
  col = c(rep("red"), rep("blue"), lty = c(1:1)))

legend("topleft", legend = c("Acessos simultaneos", "Falhas simultaneas"), col = c(rep("red"), rep("blue")), lty = c(1:1))


# # b) Efetue um diagrama de caixa de bigodes do número diário de falhas simultâneas,
# para cada servidor (o número diário de falhas simultâneas é o número total de
# falhas simultâneas que ocorreram nesse dia). Indique quantos outliers existem por servidor.

# Ordenar data frame pelas horas
december_ordered_by_time <- arrange(dez2017, dez2017$Data, dez2017$`Início de Sessão`)
View(december_ordered_by_time)

# Obter falhas diarias de cada servidor
s8_fails_data<-filter(december_ordered_by_time, december_ordered_by_time$`Tempo de Sessão` <= 1 & december_ordered_by_time$Servidor == "vsrv8") 
daily_fails_s8 <- s8_fails_data %>% group_by(s8_fails_data$Servidor, s8_fails_data$Data) %>% summarise(nFalhas_Diarias = n());

s10_fails_data<-filter(december_ordered_by_time, december_ordered_by_time$`Tempo de Sessão` <= 1 & december_ordered_by_time$Servidor == "vsrv10") 
daily_fails_s10 <- s10_fails_data %>% group_by(s10_fails_data$Servidor, s10_fails_data$Data) %>% summarise(nFalhas_Diarias = n());

s11_fails_data<-filter(december_ordered_by_time, december_ordered_by_time$`Tempo de Sessão` <= 1 & december_ordered_by_time$Servidor == "vsrv11") 
daily_fails_s11 <- s11_fails_data %>% group_by(s11_fails_data$Servidor, s11_fails_data$Data) %>% summarise(nFalhas_Diarias = n());

s16_fails_data<-filter(december_ordered_by_time, december_ordered_by_time$`Tempo de Sessão` <= 1 & december_ordered_by_time$Servidor == "vsrv16") 
daily_fails_s16 <- s16_fails_data %>% group_by(s16_fails_data$Servidor, s16_fails_data$Data) %>% summarise(nFalhas_Diarias = n());

s17_fails_data<-filter(december_ordered_by_time, december_ordered_by_time$`Tempo de Sessão` <= 1 & december_ordered_by_time$Servidor == "vsrv17") 
daily_fails_s17 <- s17_fails_data %>% group_by(s17_fails_data$Servidor, s17_fails_data$Data) %>% summarise(nFalhas_Diarias = n());

# Construir data frame com as falhas diarias de cada servidor
daily_server_fails <- data.frame(cbind(vsvr8 = daily_fails_s8$nFalhas_Diarias,
                                       vsvr10 = daily_fails_s10$nFalhas_Diarias,
                                       vsvr11 = daily_fails_s11$nFalhas_Diarias,
                                       vsvr16 = daily_fails_s16$nFalhas_Diarias,
                                       vsvr17 = daily_fails_s17$nFalhas_Diarias))
View(daily_server_fails)

# Construir boxplot
falhas_simultaneas_diarias <- boxplot(daily_server_fails, main = "Número diário de falhas simultâneas", ylab = "Número de Falhas Simultâneas");


# c) Verifique se há correlação entre o número de falhas simultâneas e o
# número de acessos simultâneos, no dia 11 de dezembro de 2017 das 12:00 às 14:00.

# data frame filtrado com apenas o dia 11 de dezembro e intervalo de tempo das 12h-14h
df_dez11 <- filter(dez2017, dez2017$Data == "2017-12-11" & 
                     dez2017$`Início de Sessão` >= "12:00" &
                     dez2017$`Início de Sessão` <= "14:00" & 
                     dez2017$`Fim de Sessão` >= "12:00" &
                     dez2017$`Fim de Sessão` <= "14:00")
View(df_dez11)

# Criar data frame vazio de falhas e acessos simultaneos do dia 11 de dezembro
df_falhas_acessos_dez11 = data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_falhas_acessos_dez11) <- c('Minuto', 'Num_Falhas_Simultaneas', 'Num_Acessos_Simultaneos')

# Extrair e ordenar os todos os tempos do data frame anterior
all_minutes_in_dez11 <- unique(c(df_dez11$`Início de Sessão`, df_dez11$`Fim de Sessão`)) %>% sort()

for(min in all_minutes_in_dez11){
  
  # Calcular acessos simultaneos 
  nFalhas_simultaneas_dez11 <-
    filter(df_dez11, df_dez11$`Tempo de Sessão` > 1,
           df_dez11$`Início de Sessão` <= min & df_dez11$`Fim de Sessão` >= min) %>% summarise(nAcessos_dez11 = n())
  dez11_accesses = sum(nFalhas_simultaneas_dez11$nAcessos_dez11)
  
  # Calcular falhas simultaneas 
  nFalhas_simultaneas_dez11 <- 
    filter(df_dez11, df_dez11$`Tempo de Sessão` <= 1,
           df_dez11$`Início de Sessão` == min | df_dez11$`Fim de Sessão` == min) %>% summarise(nFalhas_dez11 = n());
  dez11_fails = sum(nFalhas_simultaneas_dez11$nFalhas_dez11)
  
  # Rbind por linhas do data frame vazio criado com a nova informacao percorrida
  df_falhas_acessos_dez11 <- rbind(df_falhas_acessos_dez11,
                                   data.frame(Tempo = min,
                                              Num_Falhas_Simultaneas = dez11_fails,
                                              Num_Acessos_Simultaneos = dez11_accesses))
}
View(df_falhas_acessos_dez11)

# Teste de Correlação Linear de Pearson

  # Verificar se existem outliers significativos
boxplot(df_falhas_acessos_dez11[2:3], horizontal = TRUE,
        main = "Falhas/Acessos Simultaneos 11/12", xlab = "Numero de Falhas/Acessos",
        names = c("Falhas", "Acessos"))

  # Conclui-se que existem dois outliers significativos pelo que o Teste de Pearson é descartado.
  
# Teste de Correlação Ordinal de Spearman
  
  # Usado quando as variáveis são ambas ordinais ou quando uma das variáveis é contínua e a outra é ordinal
  cor.test(df_falhas_acessos_dez11$Num_Acessos_Simultaneos, 
            df_falhas_acessos_dez11$Num_Falhas_Simultaneas, 
            alternative ="two.sided", method ="spearman")
  # p-value = 0.4336
  # rho (ρ) = 0.1716352  
  
  # Como ρ está mais proximo de 0, as variaveis de acessos simultaneos e falhas simultaneas do dia 11 de dezembro
  # das 12h-14h estao fracamente correlacionadas.
  
  # Existe uma fraca associação positiva entre as classificações
  # (ρ = 0.2). Contudo esta associação não é significativa (p-value = 0.43).
  