#Problem 3

binomial <- function(n,k)
{
  stopifnot(class(n)== "numeric" && class(k)=="numeric")
  numerator <- 1
  for (i in 1:n) {
    numerator <- numerator*i
  }
  denominator1 <- 1
  for (i in 1:k) {
    denominator1 <- denominator1*i
  }
  denominator2 <- 1
  for (i in 1:(n-k)) {
    denominator2 <- denominator2*i
  }
  b <- numerator/(denominator1*denominator2)
  b
}

# Complexity of the function is O(n) (big O of n)

library(testthat)

test_that("Lets's see",
          {
    a <- binomial(4,2)
    b <- choose(4,2)
    expect_equal(a,b)
          })