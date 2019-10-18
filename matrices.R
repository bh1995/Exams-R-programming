matrices <- function(m1,m2)
{
  stopifnot(class(m1)=="matrix" || class(m2)=="matrix")
  V<-matrix(0,nrow = nrow(m1), ncol = ncol(m2))
  for (i in 1:nrow(m1))
  {
    for (e in 1:ncol(m2))
    {
      for (j in 1:ncol(m1))
      {
        V[i,e]<-V[i,e]+m1[i,j]*m2[j,e]
      }
    }
  }
  V
}    

library(testthat)

test_that("Checking if equal",{
  A<-matrix(1,nrow=3,ncol=3)
  B<-matrix(1:6,nrow=3)
  C<-matrices(A,B)
  D<-A%*%B
  expect_equal(C,D)  
  
})

test_that("hei",{
  expect_error()
  
})