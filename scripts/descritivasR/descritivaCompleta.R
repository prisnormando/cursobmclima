# Carrega os pacotes
library(readr)
library(ggplot2)

# Carrega o dataframe
dados <- read_csv("data/ETLSIM.DORES_AC_2002_t.csv")

#--->INSPEÇÃO<--
# Imprime os primeiros registros e os nomes das colunas
head(dados)

# Mostra a estrutura do dataset
str(dados)

# Mostra as dimensões do dataset
dim(dados)

#--->ANÁLISE DE COMPLETUDE<--
# Cálculo de missings
missing_percentage <- colMeans(is.na(dados)) * 100
print(missing_percentage)

# Filtragem dos missings
columns_with_missing_values <- names(missing_percentage[missing_percentage > 0])
print(columns_with_missing_values)

# Amostragem de colunas com missings
if (length(columns_with_missing_values) > 20) {
  cat("Amostragem de colunas com missings")
  print(sample(columns_with_missing_values, 20))
} else {
  cat("Colunas com missings values:\n")
  print(columns_with_missing_values)
}

#--->ANÁLISE DE VARIÁVEIS<--
#Sumário das variáveis numéricas
summary(dados[, sapply(dados, is.numeric)])

# Nome das variáveis
all_columns <- names(dados)

# Identifica as colunas categóricas
categorical_columns <- all_columns[sapply(dados, function(x) is.factor(x) | is.character(x))]

# Sample de colunas categóricas se forem muitas
if (length(categorical_columns) > 20) {
  cat("Sample categóricas:\n")
  print(sample(categorical_columns, 20))
} else {
  cat("Colunas categóricas:\n")
  print(categorical_columns)
}

#--->VISUALIZAÇÃO DAS DISTRIBUIÇÕES<--
# Histograma de Idade
# Verifica se IDADE não é numérica e corrige o erro
if (!is.numeric(dados$IDADE)) {
  dados$IDADE <- as.numeric(as.character(dados$IDADE))
  cat("Converted 'IDADE' to numeric.\n")  # Optional message
}

suppressWarnings({
  # Histograma de Idade
  ggplot(dados, aes(x = IDADE)) +
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +  # Ajuste binwidth conforme necessário
    labs(title = "Histograma de Idade", x = "Idade", y = "Contagem")
})


# Histograma de Idade da Mãe (considerando apenas valores não ausentes)
ggplot(dados[!is.na(dados$IDADEMAE), ], aes(x = IDADEMAE)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +  # Ajuste binwidth conforme necessário
  labs(title = "Histograma de Idade da Mãe", x = "Idade da Mãe", y = "Contagem")

# Histograma de Quantidade de Filhos Vivos (considerando apenas valores não ausentes)
ggplot(dados[!is.na(dados$QTDFILVIVO), ], aes(x = QTDFILVIVO)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") + 
  labs(title = "Histograma de Quantidade de Filhos Vivos", x = "Quantidade de Filhos Vivos", y = "Contagem")

# Histograma de Quantidade de Filhos Mortos (considerando apenas valores não ausentes)
ggplot(dados[!is.na(dados$QTDFILMORT), ], aes(x = QTDFILMORT)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") + 
  labs(title = "Histograma de Quantidade de Filhos Mortos", x = "Quantidade de Filhos Mortos", y = "Contagem")








