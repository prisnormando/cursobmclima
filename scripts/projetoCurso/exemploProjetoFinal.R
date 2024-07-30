#Instala e configura o remotes
install.packages('remotes')
library(remotes)

#Instala os pacotes de leitura de arquivo e ciência de dados
remotes::install_github("rfsaldanha/microdatasus")
install.packages('tidyverse')
install.packages('readr')

#Chama as bibliotecas
library(microdatasus)
library(tidyverse)
library(readr)

#Configura as pastas de trabalho
getwd()
setwd('[caminho da pasta de trabalho onde desejamos trabalhar]')

#Baixa e processa os arquivos com os dados
#dados das estações
estacoes <- read_csv("/cloud/project/projetosR/stations.csv")
#dados do SIM
sim <- fetch_datasus(year_start = 2010,
                     year_end = 2010,
                     uf = "DF",
                     information_system = "SIM-DO")

#Função do pacote microdatasus para pré-processar os dados coletados
sim_df <- process_sim(sim)

#Lê o cabeçalho dos bancos de dados
head(estacoes)
head(sim)

#Sumariza o conjunto de dados
summary(estacoes)
summary(sim)

#Verifica a estrutura dos dados
str(estacoes)
str(sim_df)


# Criar um dataframe para armazenar as taxas de mortalidade
taxas_mortalidade <- data.frame(
  faixa_etaria = c("0-19 anos", "20-39 anos", "40-59 anos", "60 anos ou mais"),
  obitos = c(0, 0, 0, 0),
  populacao = c(1000000, 1000000, 1000000, 1000000),
  taxa_mortalidade = c(0, 0, 0, 0)
)

# Calcular a taxa de mortalidade por faixa etária
for (i in 1:nrow(df_sim)) {
  idade <- df_sim$IDADE[i]
  
  if (idade >= 0 && idade <= 19) {
    taxas_mortalidade$obitos[1] <- taxas_mortalidade$obitos[1] + 1
  } else if (idade >= 20 && idade <= 39) {
    taxas_mortalidade$obitos[2] <- taxas_mortalidade$obitos[2] + 1
  } else if (idade >= 40 && idade <= 59) {
    taxas_mortalidade$obitos[3] <- taxas_mortalidade$obitos[3] + 1
  } else {
    taxas_mortalidade$obitos[4] <- taxas_mortalidade$obitos[4] + 1
  }
}

# Calcular a taxa de mortalidade por 100.000 habitantes
taxas_mortalidade$taxa_mortalidade <- (taxas_mortalidade$obitos / taxas_mortalidade$populacao) * 100000

# Exibir os resultados
print(taxas_mortalidade)


