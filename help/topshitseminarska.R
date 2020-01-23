ucna <- read.table("AlgaeLearn.txt", header = T)
summary(ucna)

lm.model <- lm(a1 ~ ., data = ucna)

predisted <- predict(lm.model, testna)
observed <- testna$a1

mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

mae(observed, predicted)

rmae <- function(observed, predicted, mean.val)
{
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}
