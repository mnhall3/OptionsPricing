
Test_Cases <- 1000

Random_Var1 <- sample(c(1, -1), Test_Cases, replace = TRUE)

prob_match <- 0.9
Random_Var2 <- sapply(Random_Var1, function(xi) {
  if (xi == 1) {
    sample(c(1, -1), 1, prob = c(prob_match, 1 - prob_match))
  } else {
    sample(c(1, -1), 1, prob = c(1 - prob_match, prob_match))
  }
})
cor(data$Random_Var1, data$Random_Var2)
data <- data.frame(Random_Var1, Random_Var2)



