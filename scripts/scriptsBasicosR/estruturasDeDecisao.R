# if-else
x <- 10
if (x > 0) {
  print("x é positivo")
} else {
  print("x é negativo")
}

# if-else if-else
score <- 85
if (score >= 90) {
  print("Excelente")
} else if (score >= 80) {
  print("Bom")
} else {
  print("Precisa melhorar")
}

# switch
day <- 3
day_name <- switch(day,
                   "Segunda-feira",
                   "Terça-feira",
                   "Quarta-feira",
                   "Quinta-feira",
                   "Sexta-feira",
                   "Sábado",
                   "Domingo")
print(day_name)

# ifelse
temperatures <- c(20, 25, 18, 30)
weather_status <- ifelse(temperatures > 25, "Quente", "Frio")
print(weather_status)

# case_when
grades <- c(85, 92, 75, 88)
letter_grades <- case_when(
  grades >= 90 ~ "A",
  grades >= 80 ~ "B",
  grades >= 70 ~ "C",
  TRUE ~ "D"
)
print(letter_grades)