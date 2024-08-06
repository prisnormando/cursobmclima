

# Carrega os pacotes
library(readr)

# Carrega o dataframe
dados <- read_csv("data/ETLSIM.DORES_AC_2002_t.csv")

# Print summary statistics for numeric columns
cat("\nColunas numéricas:\n")
print(summary(dados[, sapply(dados, is.numeric)]))

# Print frequency tables for categorical columns
cat("\nFrequência das variávies categóricas:\n")
for (col in names(dados)) {
  if (is.factor(dados[[col]]) | is.character(dados[[col]])) {
    freq_table <- table(dados[[col]], exclude = NULL)
    
    # If there are too many unique values, sample 20
    if (length(freq_table) > 20) {
      cat(paste("Column:", col, "- Sampled 20 unique values\n"))
      print(sample(freq_table, 20))
    } else {
      cat(paste("Column:", col, "\n"))
      print(freq_table)
    }
    cat("\n")
  }
}
