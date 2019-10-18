mult_for <- function(A, B) {
    rows <- nrow(A)
    cols <- ncol(B)

    noise <- rnorm(rows * cols)
    V <- matrix(noise, nrow = rows, ncol = cols)
    for (i in 1:rows) {
        for (j in 1:cols) {
            V[i,j] <- sum( A[i,] * B[,j] )
        }
    }
    return(V)
}


## Unit test ##
library(testthat)
context("exam_20181023_p3")

A <- matrix(c(1:9), nrow = 3)
B <- matrix(c(11:19), nrow = 3)

test_that("correct answer", {
    expect_equal(mult_for(A, B), A %*% B)
}
)
