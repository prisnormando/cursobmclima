#Instala os pacotes de leitura de arquivo e ciência de dados
install.packages("tidyverse")
install.packages("readr")

#Chama as bibliotecas 
library(tidyverse)
library(readr)

#Lê o arquivo de dados
estacoes <- read_csv('stations.csv')

#Sumariza o conjunto de dados
summary(estacoes)

