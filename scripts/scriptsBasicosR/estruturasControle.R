for (i in 1:5) {
  print(i)
}

i <- 1
while (i <= 5) {
  print(i)
  i <- i + 1
}

i <- 1
repeat {
  print(i)
  i <- i + 1
  if (i > 5) {
    break
  }
}

numbers <- c(1, 2, 3, 4, 5)
squared <- sapply(numbers, function(x) x^2)

matrix <- matrix(1:6, nrow = 2)
row_sums <- apply(matrix, 1, sum)


