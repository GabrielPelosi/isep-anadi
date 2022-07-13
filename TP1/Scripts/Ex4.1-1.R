# 4.1. Análise do funcionamento dos servidores VPN do DEI

# a) Falha - embora a razão de uma sessão ter uma curta duração possa ter diversas origens, para o efeito
#   deste estudo considera-se que qualquer sessão com duração igual ou inferior a um minuto representa uma falha.
# b) Número de falhas simultâneas - Em cada minuto t0, define-se o número de
#   falhas simultâneas como sendo o número de falhas que inicializaram ou que finalizaram no minuto t0.

# c) Acesso - Um acesso é uma sessão com duração superior a um minuto.
# d) Número de acessos simultâneos - Em cada minuto t0, define-se o número de
#   acessos simultâneos como sendo o número de acessos que inicializaram antes do minuto t0 e terminaram depois do minuto t0.

# ===================================== Ex1 ====================================

# Import data from vpnsessionsfile.txt file
vpn_sessions <- read.table(file = "Data/vpnsessionsfile.txt", header = F)
View(vpn_sessions)

# Remove unused columns
vpn_sessions$V6 <- NULL
vpn_sessions$V8 <- NULL

# Change column names
colnames(vpn_sessions)[1] <- "Servidor"
colnames(vpn_sessions)[2] <- "Protocolo"
colnames(vpn_sessions)[3] <- "Data"
colnames(vpn_sessions)[4] <- "Início de Sessão"
colnames(vpn_sessions)[5] <- "Fim de Sessão"
colnames(vpn_sessions)[6] <- "Tempo de Sessão"

# Converter Strings de data em Datas
datas_sessoes <- as.Date(vpn_sessions$Data, "%Y-%m-%d"); datas_sessoes

# Numero total de linhas do ficheiro (66383)
file_size <- length(vpn_sessions$Servidor); file_size
summary(vpn_sessions)

# Numero de vezes cada servidor aparece no ficheiro
library("plyr")
count(vpn_sessions$Servidor)

# ========================== Definições ==========================
# a) Falha - embora a razão de uma sessão ter uma curta duração possa ter diversas origens, para o efeito
#   deste estudo considera-se que qualquer sessão com duração igual ou inferior a um minuto representa uma falha.
# b) Número de falhas simultâneas - Em cada minuto t0, define-se o número de
#   falhas simultâneas como sendo o número de falhas que inicializaram ou que finalizaram no minuto t0.

# c) Acesso - Um acesso é uma sessão com duração superior a um minuto.
# d) Número de acessos simultâneos - Em cada minuto t0, define-se o número de
#   acessos simultâneos como sendo o número de acessos que inicializaram antes do minuto t0 e terminaram depois do minuto t0.

# ===================================== Ex1 ====================================

# =================== a) Quantos acessos teve cada servidor? ===================

# Para cada servidor verificar quantas linhas com sessoes superiores a 1 minuto existem

# Numero de acessos do servidor vsrv10
accesses_s10 <- sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1)
sprintf("O servidor 'vsrv10' teve %d acessos", accesses_s10)

# Numero de acessos do servidor vsrv11
accesses_s11 <- sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1)
sprintf("O servidor 'vsrv11' teve %d acessos", accesses_s11)

# Numero de acessos do servidor vsrv16
accesses_s16 <- sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1)
sprintf("O servidor 'vsrv16' teve %d acessos", accesses_s16)

# Numero de acessos do servidor vsrv17
accesses_s17 <- sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1)
sprintf("O servidor 'vsrv17' teve %d acessos", accesses_s17)

# Numero de acessos do servidor vsrv8
accesses_s8 <- sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1)
sprintf("O servidor 'vsrv8' teve %d acessos", accesses_s8)


# b) =================== Quantas falhas teve cada servidor? ==================== 

# Para cada servidor verificar quantas linhas com sessoes iguais ou inferiores a um minuto existem

# Numero de falhas do servidor vsrv10
fails_s10 <- sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` <= 1)

sprintf("O servidor 'vsrv10' teve %d falhas", fails_s10)

# Numero de falhas do servidor vsrv11
fails_s11 <- sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` <= 1)

sprintf("O servidor 'vsrv11' teve %d falhas", fails_s11)

# Numero de falhas do servidor vsrv16
fails_s16 <- sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` <= 1)
sprintf("O servidor 'vsrv16' teve %d falhas", fails_s16)

# Numero de falhas do servidor vsrv17
fails_s17 <- sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` <= 1)
sprintf("O servidor 'vsrv17' teve %d falhas", fails_s17)

# Numero de falhas do servidor vsrv8
fails_s8 <- sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` <= 1)
sprintf("O servidor 'vsrv8' teve %d falhas", fails_s8)


# ============ c) Quantas vezes o servidor ”x” usa o protocolo ”y”? ============

print("Server names: vsrv8; vsrv10; vsrv11; vsrv16; vsrv17")
serverX <- readline(prompt = "Enter server name: ")

print("VPN Protocol names: PPTP; SSTP; SOFTETHER; OPENVPN_L2; OPENVPN_L3")
protocolY <- readline(prompt = "Enter VPN protocol: ")

nTimes_ServerX_ProtocolY <- sum(vpn_sessions$Servidor == serverX &
                                  vpn_sessions$Protocolo == protocolY)

sprintf("The server '%s' uses the VPN protocol '%s', %d times.", serverX,
        protocolY,
        nTimes_ServerX_ProtocolY)

# ========= Outra alternativa de modo a sumarizar os usos para cada servidor ==========

protocol_name <- c("PPTP", "SSTP", "SOFTETHER", "OPENVPN L2", "OPENVPN L3")

# Tabela que mostra o numero de vezes que o servidor vsrv8 usa cada protocolo
vsrv8_uses <- c(sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$Protocolo == "PPTP"), 
                   sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$Protocolo == "SSTP"), 
                   sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$Protocolo == "SOFTETHER"), 
                   sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$Protocolo == "OPENVPN_L2"), 
                   sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$Protocolo == "OPENVPN_L3"))

# Tabela que mostra o numero de vezes que o servidor vsrv10 usa cada protocolo
vsrv10_uses <- c(sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$Protocolo == "PPTP"), 
                sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$Protocolo == "SSTP"), 
                sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$Protocolo == "SOFTETHER"), 
                sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$Protocolo == "OPENVPN_L2"), 
                sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$Protocolo == "OPENVPN_L3"))

# Tabela que mostra o numero de vezes que o servidor vsrv11 usa cada protocolo
vsrv11_uses <- c(sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$Protocolo == "PPTP"), 
                 sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$Protocolo == "SSTP"), 
                 sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$Protocolo == "SOFTETHER"), 
                 sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$Protocolo == "OPENVPN_L2"), 
                 sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$Protocolo == "OPENVPN_L3"))

# Tabela que mostra o numero de vezes que o servidor vsrv16 usa cada protocolo
vsrv16_uses <- c(sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$Protocolo == "PPTP"), 
                 sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$Protocolo == "SSTP"), 
                 sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$Protocolo == "SOFTETHER"), 
                 sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$Protocolo == "OPENVPN_L2"), 
                 sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$Protocolo == "OPENVPN_L3"))

# Tabela que mostra o numero de vezes que o servidor vsrv17 usa cada protocolo
vsrv17_uses <- c(sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$Protocolo == "PPTP"), 
                 sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$Protocolo == "SSTP"), 
                 sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$Protocolo == "SOFTETHER"), 
                 sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$Protocolo == "OPENVPN_L2"), 
                 sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$Protocolo == "OPENVPN_L3"))

serverX_uses_protocolY <- data.frame(protocol_name, vsrv8_uses, vsrv10_uses, vsrv11_uses, vsrv16_uses, vsrv17_uses)
View(serverX_uses_protocolY)


# d) Determine as médias, medianas e desvios padrão mensais de acessos de cada servidor

# ==== acessos de meses completos do servidor vsvr8 ====
accesses_2017_s8<-c(
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-01-01" & vpn_sessions$Data <= "2017-01-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-02-01" & vpn_sessions$Data <= "2017-02-28"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-03-01" & vpn_sessions$Data <= "2017-03-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-04-01" & vpn_sessions$Data <= "2017-04-30"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-05-01" & vpn_sessions$Data <= "2017-05-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-06-01" & vpn_sessions$Data <= "2017-06-30"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-07-01" & vpn_sessions$Data <= "2017-07-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-08-01" & vpn_sessions$Data <= "2017-08-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-09-01" & vpn_sessions$Data <= "2017-09-30"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-10-01" & vpn_sessions$Data <= "2017-10-31"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-11-01" & vpn_sessions$Data <= "2017-11-30"),
  sum(vpn_sessions$Servidor == "vsrv8" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-12-01" & vpn_sessions$Data <= "2017-12-31"))

# ==== acessos de meses completos do servidor vsrv10 ====
accesses_2017_s10<-c(
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-01-01" & vpn_sessions$Data <= "2017-01-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-02-01" & vpn_sessions$Data <= "2017-02-28"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-03-01" & vpn_sessions$Data <= "2017-03-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-04-01" & vpn_sessions$Data <= "2017-04-30"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-05-01" & vpn_sessions$Data <= "2017-05-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-06-01" & vpn_sessions$Data <= "2017-06-30"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-07-01" & vpn_sessions$Data <= "2017-07-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-08-01" & vpn_sessions$Data <= "2017-08-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-09-01" & vpn_sessions$Data <= "2017-09-30"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-10-01" & vpn_sessions$Data <= "2017-10-31"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-11-01" & vpn_sessions$Data <= "2017-11-30"),
  sum(vpn_sessions$Servidor == "vsrv10" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-12-01" & vpn_sessions$Data <= "2017-12-31"))

# ==== acessos de meses completos do servidor vsrv11 ====
accesses_2017_s11<-c(
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-01-01" & vpn_sessions$Data <= "2017-01-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-02-01" & vpn_sessions$Data <= "2017-02-28"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-03-01" & vpn_sessions$Data <= "2017-03-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-04-01" & vpn_sessions$Data <= "2017-04-30"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-05-01" & vpn_sessions$Data <= "2017-05-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-06-01" & vpn_sessions$Data <= "2017-06-30"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-07-01" & vpn_sessions$Data <= "2017-07-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-08-01" & vpn_sessions$Data <= "2017-08-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-09-01" & vpn_sessions$Data <= "2017-09-30"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-10-01" & vpn_sessions$Data <= "2017-10-31"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-11-01" & vpn_sessions$Data <= "2017-11-30"),
  sum(vpn_sessions$Servidor == "vsrv11" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-12-01" & vpn_sessions$Data <= "2017-12-31"))

# ==== acessos de meses completos do servidor vsrv16 ====
accesses_2017_s16<-c(
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-01-01" & vpn_sessions$Data <= "2017-01-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-02-01" & vpn_sessions$Data <= "2017-02-28"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-03-01" & vpn_sessions$Data <= "2017-03-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-04-01" & vpn_sessions$Data <= "2017-04-30"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-05-01" & vpn_sessions$Data <= "2017-05-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-06-01" & vpn_sessions$Data <= "2017-06-30"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-07-01" & vpn_sessions$Data <= "2017-07-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-08-01" & vpn_sessions$Data <= "2017-08-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-09-01" & vpn_sessions$Data <= "2017-09-30"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-10-01" & vpn_sessions$Data <= "2017-10-31"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-11-01" & vpn_sessions$Data <= "2017-11-30"),
  sum(vpn_sessions$Servidor == "vsrv16" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-12-01" & vpn_sessions$Data <= "2017-12-31"))

# ==== acessos de meses completos do servidor vsrv17 ====
accesses_2017_s17<-c(
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-01-01" & vpn_sessions$Data <= "2017-01-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-02-01" & vpn_sessions$Data <= "2017-02-28"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-03-01" & vpn_sessions$Data <= "2017-03-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-04-01" & vpn_sessions$Data <= "2017-04-30"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-05-01" & vpn_sessions$Data <= "2017-05-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-06-01" & vpn_sessions$Data <= "2017-06-30"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-07-01" & vpn_sessions$Data <= "2017-07-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-08-01" & vpn_sessions$Data <= "2017-08-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-09-01" & vpn_sessions$Data <= "2017-09-30"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-10-01" & vpn_sessions$Data <= "2017-10-31"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-11-01" & vpn_sessions$Data <= "2017-11-30"),
  sum(vpn_sessions$Servidor == "vsrv17" & vpn_sessions$`Tempo de Sessão` > 1 & vpn_sessions$Data >= "2017-12-01" & vpn_sessions$Data <= "2017-12-31"))

# ==== MÉDIA MENSAL DE ACESSOS DE CADA SERVIDOR
sprintf("O servidor 'vsrv8' tem uma média mensal de acessos aproximadamente igual a %g.", mean(accesses_2017_s8))
sprintf("O servidor 'vsrv10' tem uma média mensal de acessos aproximadamente igual a %g.", mean(accesses_2017_s10))
sprintf("O servidor 'vsrv11' tem uma média mensal de acessos aproximadamente igual a %g.", mean(accesses_2017_s11))
sprintf("O servidor 'vsrv16' tem uma média mensal de acessos aproximadamente igual a %g.", mean(accesses_2017_s16))
sprintf("O servidor 'vsrv17' tem uma média mensal de acessos aproximadamente igual a %g.", mean(accesses_2017_s17))

# ==== MEDIANA MENSAL DE ACESSOS DE CADA SERVIDOR
sprintf("O servidor 'vsrv8' tem uma mediana mensal de acessos aproximadamente igual a %g.", median(accesses_2017_s8))
sprintf("O servidor 'vsrv10' tem uma mediana mensal de acessos aproximadamente igual a %g.", median(accesses_2017_s10))
sprintf("O servidor 'vsrv11' tem uma mediana mensal de acessos aproximadamente igual a %g.", median(accesses_2017_s11))
sprintf("O servidor 'vsrv16' tem uma mediana mensal de acessos aproximadamente igual a %g.", median(accesses_2017_s16))
sprintf("O servidor 'vsrv17' tem uma mediana mensal de acessos aproximadamente igual a %g.", median(accesses_2017_s17))

# ==== DESVIO PADRAO MENSAL DE ACESSOS DE CADA SERVIDOR
sprintf("O servidor 'vsrv8' tem um desvio padrao mensal de acessos aproximadamente igual a %g.", sd(accesses_2017_s8))
sprintf("O servidor 'vsrv10' tem um desvio padrao mensal de acessos aproximadamente igual a %g.", sd(accesses_2017_s10))
sprintf("O servidor 'vsrv11' tem um desvio padrao mensal de acessos aproximadamente igual a %g.", sd(accesses_2017_s11))
sprintf("O servidor 'vsrv16' tem um desvio padrao mensal de acessos aproximadamente igual a %g.", sd(accesses_2017_s16))
sprintf("O servidor 'vsrv17' tem um desvio padrao mensal de acessos aproximadamente igual a %g.", sd(accesses_2017_s17))

