# Instalação de pacotes
install.packages('remotes')
library(remotes)

# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

#Instalar e chamar
Quarto
Shiny


library(tidyverse)

ENTRADA -> PROCESSAMENTO -> SAÍDA
# variáveis
x <- 5
y <- 11

#constantes
PI <- 3.14


# operadores
g <- x * y

#funcoes
z <- c(12,3,5,9,0)
w <- c('a', 'b', 'c', 'd', '13')
d <- c(x, y, g, z)
f <- c('c', 'a', 's', 'a')


help(c)

ll <- list(A = 1, c = "C")
print(ll, , quote = FALSE)

help(print)

set.seed(521)
t1 <- round(abs(rt(200, df = 1.8)))
t2 <- round(abs(rt(200, df = 1.4)))
table(t1, t2) # simple
print(table(t1, t2), zero.print = ".")

help(microdatasus)

stations <- read.csv("/cloud/project/stations.csv", encoding = "UTF-8")

help("read.csv")








