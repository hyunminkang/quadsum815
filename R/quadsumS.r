#' A loop-free R implementation for computing x'Ay
#'
#' @param x A size n vector
#' @param y A size m vector
#' @param A A size (n x m) matrix
#' @return A scalar value evaluating x'Ay
#' @export
quadsumS <- function(x, y, A) { # function definition // HL
    n <- length(x)
    m <- length(y)
    stopifnot(dim(A) == c(n,m)) # check whether the dimensions match
    sum <- 0
    return( sum( matrix(x,n,m) * A * matrix(y,n,m,byrow=TRUE) ) ) ## element-wise sums // HL
}
