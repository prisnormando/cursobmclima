########################################
#               CURSO FIOCRUZ          #
# AULA PRATICA - ANALISE DE CORRELACAO #  
########################################  

library(ggplot2)
library(corrplot)

##############################
# 1. Carregar a base de dado #
##############################
merged <- read.csv("C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/merged_data.csv")
head(merged)
summary(merged)

############################
# 2. Analise de correlacao #
############################
# see reference: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#correlation-coefficient

## 2.1. Diagrama de dispersao
# N_interna SIH x temp
ggplot(merged) +
  aes(x = temp, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# N_interna SIH x precipitacao
ggplot(merged) +
  aes(x = precip, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# N_interna SIH x umidade
ggplot(merged) +
  aes(x = umid, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# N_interna SIH x pressao atmosferica
ggplot(merged) +
  aes(x = pa, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# N_interna SIH x vento
ggplot(merged) +
  aes(x = vento, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# N_interna SIH x radiacao
ggplot(merged) +
  aes(x = radiacao, y = n_interna) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# Todos os graficos de dispersao juntos:
pairs(merged[, c("n_interna", "temp", "precip", "umid", "pa", "vento", "radiacao")])


# Grafico de dispersao por # Scatter plot
plot(merged$umid, merged$n_interna, 
     pch = 19,
     col = factor(merged$Municipio), xlab = "Umidade Relativa (%)",
     ylab = "N de hospitalizacoes")

legend("topleft",
       legend = levels(factor(merged$Municipio)),
       pch = 19,
       col = factor(levels(factor(merged$Municipio))))

# plot a regression line 
abline(lm(n_interna ~ umid, data = merged),col='red')

# Legend
legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group)))) 


## 2.2. Matriz de correlacao
round(cor(merged[,c(4:11)]),
      digits = 2 # rounded to 2 decimals
)
corrplot(cor(merged[,c(4:7,9:11)]),
         method = "number",
         type = "upper" # show only upper side))

## 2.3. Teste de correlacao (Pearson) - n_interna x precip, umid, pa
cor.test(merged$n_interna, merged$precip)
cor.test(merged$n_interna, merged$umid)
cor.test(merged$n_interna, merged$pa)

# 2.4. Teste de correlacao (Spearman) - n_interna x precip, umid, pa
cor.test(merged$n_interna, merged$precip, method = "spearman")
cor.test(merged$n_interna, merged$umid, method = "spearman")
cor.test(merged$n_interna, merged$pa, method = "spearman")


