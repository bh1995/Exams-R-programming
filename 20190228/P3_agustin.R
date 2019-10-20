
# The fastest way to implement is using built-in factorial function
# Though, it could be easily written by using recursive algorithms
# Bad memory handling in R would end up in horrible performance for large cases

# slow factorial:
# my_factorial <- function(n) {
#     if (n == 0) {
#         return (1)
#     } else {
#         return(my_factorial(n-1) * n)
#     }
# }

bin_coeff <- function (n,k) {
    stopifnot(class(n) == "numeric")
    stopifnot(class(k) == "numeric")
    if (n < k) {
        return(0)
    }
    return(factorial(n) / (factorial(n-k) * factorial(k)))
}


library(testthat)
test_that( "binary_coefficient", {
    # true positive
    expect_equal(bin_coeff(6,4) , choose(6,4)) 
    expect_equal(bin_coeff(23,5) , choose(23,5)) 
    
    # bad type handling
    expect_error(bin_coeff(5, "a"))
    expect_error(bin_coeff("a", 5))
    
    # border cases
    expect_equal(bin_coeff(3,3), 1)
    expect_equal(bin_coeff(3,4), 0)
}
)