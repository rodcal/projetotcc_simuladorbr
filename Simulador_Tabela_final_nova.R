require(readxl)
require(ggcorrplot)
require(dplyr)
require(readr)
#Lê excel
Base <- read_excel("~/Projeto Simulação Campeonato BR/Base.xlsx")
historico_jogos <- read_csv("~/Projeto Simulação Campeonato BR/historico_jogos.txt")


# Nomeia cabeçalhos e transformar em dataframe
df <- Base
df <- as.data.frame(df)
rownames(df) <-  c('America-MG','Athletico - PR','Atletico-MG','Bahia','Botafogo','Bragantino','Corinthians','Coritiba','Cruzeiro','Cuiaba','Flamengo','Fluminense','Fortaleza','Goias','Gremio','Internacional','Palmeiras','Santos','Sao Paulo','Vasco')

# Função para calcular o histórico de confrontos entre dois times
calcula_historico_confrontos <- function(TimeA, TimeB, historico_jogos) {
  confrontos <- historico_jogos %>%
    filter((mandante == TimeA & visitante == TimeB) | (mandante == TimeB & visitante == TimeA))
  
  vitorias_timeA <- sum(confrontos$vencedor == TimeA)
  vitorias_timeB <- sum(confrontos$vencedor == TimeB)
  empates <- sum(confrontos$vencedor == "-")
  
  return(c(vitorias_timeA, vitorias_timeB, empates))
}

# Função para calcular o desempenho como mandante e visitante
calcula_desempenho_casa_fora <- function(Time, historico_jogos) {
  jogos_casa <- historico_jogos %>%
    filter(mandante == Time)
  
  jogos_fora <- historico_jogos %>%
    filter(visitante == Time)
  
  if (nrow(jogos_casa) == 0) {
    vitorias_casa <- 0
  } else {
    vitorias_casa <- sum(jogos_casa$vencedor == Time) / nrow(jogos_casa)
  }
  
  if (nrow(jogos_fora) == 0) {
    vitorias_fora <- 0
  } else {
    vitorias_fora <- sum(jogos_fora$vencedor == Time) / nrow(jogos_fora)
  }
  
  return(c(vitorias_casa, vitorias_fora))
}

# Função para calcular o saldo de gols como mandante e visitante
calcula_saldo_casa_fora <- function(Time, historico_jogos) {
  jogos_casa <- historico_jogos %>%
    filter(mandante == Time)
  
  jogos_fora <- historico_jogos %>%
    filter(visitante == Time)
  
  if (nrow(jogos_casa) == 0) {
    saldo_casa <- 0
  } else {
    saldo_casa <- sum(jogos_casa$mandante_Placar) / nrow(jogos_casa)
  }
  
  if (nrow(jogos_fora) == 0) {
    saldo_fora <- 0
  } else {
    saldo_fora <- sum(jogos_fora$visitante_Placar) / nrow(jogos_fora)
  }
  
  return(c(saldo_casa, saldo_fora))
}



# Função Simulação de um jogo (atualizada)
simula_jogo <- function(TimeA, TimeB, historico_jogos) {
  # Pesos para cada métrica
  peso_elenco <- 10
  peso_historico_mandante <- 5
  peso_historico_visitante <- 5
  peso_historico_jogos <- 2
  peso_saldo_gols <- 1
  
  confronto <- calcula_historico_confrontos(TimeA, TimeB, historico_jogos)
  desempenho_casa_fora_A <- calcula_desempenho_casa_fora(TimeA, historico_jogos)
  desempenho_casa_fora_B <- calcula_desempenho_casa_fora(TimeB, historico_jogos)
  saldo_gols_A <- calcula_saldo_casa_fora(TimeA,historico_jogos)
  saldo_gols_B <- calcula_saldo_casa_fora(TimeB,historico_jogos)
  x <- sample(1:round(max(df[TimeA, 2] * peso_elenco + desempenho_casa_fora_A[1] * peso_historico_mandante + confronto[1] * peso_historico_jogos + saldo_gols_A[1] * peso_saldo_gols), 0), 1)
  y <- sample(1:round(max(df[TimeB, 2] * peso_elenco + desempenho_casa_fora_B[2] * peso_historico_visitante + confronto[2] * peso_historico_jogos + saldo_gols_B[2] * peso_saldo_gols), 0), 1)
  
  # Margem de empate baseada em uma porcentagem da pontuação máxima
  margem_empate <- 0.1605 * max(x, y)
  
  if (abs(x - y) <= margem_empate) {
    return (0)
  } else if (x > y) {
    return (1)
  } else {
    return (-1)
  }
}

#Função de simulação do campeonato
simula_campeonato <- function(df){
  Matriz_Resultados <- matrix(0, nrow= 20, ncol=6)
  for (i in 1:20){
    for (j in 1:20){
      x <- simula_jogo(rownames(df[i, ]), rownames(df[j, ]), historico_jogos)
      if (rownames(df[i,]) == rownames(df[j,])){
        next
      }
      Matriz_Resultados[i,2] = Matriz_Resultados[i,2] + 1
      Matriz_Resultados[j,2] = Matriz_Resultados[j,2] + 1
      if (x == 1){
         Matriz_Resultados[i,3] = Matriz_Resultados[i,3] + 1
         Matriz_Resultados[i,6] = Matriz_Resultados[i,6] + 3
         Matriz_Resultados[j,5] = Matriz_Resultados[j,5] + 1
      } else if (x == -1){
        Matriz_Resultados[i,5] = Matriz_Resultados[i,5] + 1
        Matriz_Resultados[j,6] = Matriz_Resultados[j,6] + 3
        Matriz_Resultados[j,3] = Matriz_Resultados[j,3] + 1
      } else if (x == 0){
        Matriz_Resultados[i,4] = Matriz_Resultados[i,4] + 1
        Matriz_Resultados[i,6] = Matriz_Resultados[i,6] + 1
        Matriz_Resultados[j,4] = Matriz_Resultados[i,4] + 1
        Matriz_Resultados[j,6] = Matriz_Resultados[j,6] + 1
      }
    }
  }
  Matriz_Resultados[,1] <- rownames(df[])
  return (Matriz_Resultados)
}

#Estrutura Tabela para o  PowerBI
Tabela_final <- simula_campeonato(df)
colnames(Tabela_final) <- c("Times","Jogos","Vitórias","Empates","Derrotas","Pontos")
Tabela_final <- as.data.frame(Tabela_final)
Tabela_final <- Tabela_final[order(Tabela_final$Pontos,decreasing = TRUE),]

#Função simula Chances
simula_chances <- function(df, historico_jogos){
  Matriz_chances <- matrix(0, nrow= 20, ncol=20)
  Matriz_chances <- Matriz_chances + 99
  for (i in 1:20){
    for (j in 1:20){
      chances = 0
      for (k in 1:10){
        x <- simula_jogo(rownames(df[i,]),rownames(df[j,]), historico_jogos)
        if (rownames(df[i,]) == rownames(df[j,])){
          chances = chances + 1
        }else if (x == 1){
          chances = chances + 1
        }else{
          chances = chances + 0
        }
        
      }
      if (Matriz_chances[i,j] != 99){
        Matriz_chances[j,i] = 10 - Matriz_chances[i,j]
      }else{
        Matriz_chances[j,i] = chances
      }
    }
  }
  return (Matriz_chances)
}

#Cria a matriz de correlação
Teste_Matriz <- simula_chances(df, historico_jogos)
rownames(Teste_Matriz) <- c('America-MG','Athletico - PR','Atletico-MG','Bahia','Botafogo','Bragantino','Corinthians','Coritiba','Cruzeiro','Cuiaba','Flamengo','Fluminense','Fortaleza','Goias','Gremio','Internacional','Palmeiras','Santos','Sao Paulo','Vasco')
colnames(Teste_Matriz) <- c('America-MG','Athletico - PR','Atletico-MG','Bahia','Botafogo','Bragantino','Corinthians','Coritiba','Cruzeiro','Cuiaba','Flamengo','Fluminense','Fortaleza','Goias','Gremio','Internacional','Palmeiras','Santos','Sao Paulo','Vasco')
Teste_Matriz <- Teste_Matriz / 10
Tabela_Correlacao <- as.data.frame(Teste_Matriz)
#Tabela_Correlacao <- cbind(Tabela_Correlacao,c('América-MG','Athletico Paranaense','Atlético-GO','Atlético-MG','Avaí','Botafogo','Bragantino','Ceará','Corinthians','Coritiba','Cuiabá','Flamengo','Fluminense','Fortaleza','Goiás','Internacional','Juventude','Palmeiras','Santos','São Paulo'))
#colnames(Tabela_Correlacao) <- c('América-MG','Athletico Paranaense','Atlético-GO','Atlético-MG','Avaí','Botafogo','Bragantino','Ceará','Corinthians','Coritiba','Cuiabá','Flamengo','Fluminense','Fortaleza','Goiás','Internacional','Juventude','Palmeiras','Santos','São Paulo','Times')

#Plot Matriz de Correlação das chances de vencer  jogo a jogo
ggcorrplot(Tabela_Correlacao, outline.col = "white", lab=TRUE, lab_size = 2.5,ggtheme = ggplot2::theme_gray,
          colors = c("#6D9EC1", "white", "#E46726"))


#Usando a Função abaixo foi constatado que de 2003 para cá, o % de empates é igual a  0.2645483
# #Calcula o % de empates no campeonato
# total_empate <- historico_jogos %>%
#   filter(historico_jogos$vencedor == "-") %>%
#   count()
# 
# total_outros <- historico_jogos %>%
#   filter(historico_jogos$vencedor != "-") %>%
#   count()
# 
# empate <- total_empate/(total_empate  + total_outros)


#Criando uma função para checar o total de empates em relação aos jogos
#com a margem em 12% o percentual de empates varia entre 20 ~ 30% o que
# total_empates <- Tabela_final
# total_empates$Empates <- as.numeric(total_empates$Empates)
# total_empates_soma <- sum(total_empates$Empates)
# calcula_percentual <- total_empates_soma/760

#Calcula o percentual médio de empate em 50 simulações
# n_simulacoes <- 50'
# empates_simulados <- vector("numeric", n_simulacoes)
# 
# for (i in 1:n_simulacoes){
#   tabela_simulada <- simula_campeonato(df)
#   colnames(tabela_simulada) <- c("Times","Jogos","Vitórias","Empates","Derrotas","Pontos")
#   tabela_simulada <- as.data.frame(tabela_simulada)
#   empates <- as.numeric(tabela_simulada$Empates)
#   empates_simulados[i] <- sum(empates)
# }
# 
# media_empates <- mean(empates_simulados)
# percentual_empates <- (media_empates / 760) * 100
# 
# rm(empates,empates_simulado,i,percentual_empates)

#Através dessa simulação foi encontrando o valor de 0.16 na margem que varia entre 24~26% de empates 

# Função para executar várias simulações e salvar os resultados
# executa_simulacoes <- function(n_simulacoes, df) {
#   resultados <- list()
#   
#   for (i in 1:n_simulacoes) {
#     tabela_final <- simula_campeonato(df)
#     colnames(tabela_final) <- c("Times", "Jogos", "Vitorias", "Empates", "Derrotas", "Pontos")
#     tabela_final <- as.data.frame(tabela_final)
#     tabela_final <- tabela_final[order(tabela_final$Pontos, decreasing = TRUE), ]
#     
#     tabela_final$Simulacao <- i
#     resultados[[i]] <- tabela_final
#   }
#   
#   return(resultados)
# }
# 
# # Executa 100 simulações e salva os resultados em um arquivo CSV
# n_simulacoes <- 100
# resultados <- executa_simulacoes(n_simulacoes, df)
# 
# # Combina os resultados em um único dataframe
# resultados_concatenados <- do.call(rbind, resultados)
# 
# # Salva os resultados em um arquivo CSV
# write.csv(resultados_concatenados, file = "resultados_simulacoes.csv", row.names = FALSE)