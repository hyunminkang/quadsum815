#' Matrix-based computation of x'Ay
#'
#' @param x A size n vector
#' @param y A size m vector
#' @param A A size (n x m) matrix
#' @return A scalar value evaluating x'Ay
#' @export
quadsumT <- function(x, y, A) {
    n <- length(x)
    m <- length(y)
    stopifnot(dim(A) == c(n,m)) # check whether the dimensions match
    return ( as.numeric(x %*% A %*% y) )  # use matrix operation // HL
}
