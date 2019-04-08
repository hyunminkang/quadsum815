#' A loop-based R implementation for computing x'Ay
#'
#' @param  x A size n vector
#' @param  y A size m vector
#' @param  A A size (n x m) matrix
#' @return A scalar value evaluating x'Ay
#' @export
quadsumR <- function(x, y, A) { # function definition // HL
    n <- length(x)
    m <- length(y)
    stopifnot(dim(A) == c(n,m)) # sanity check
    sum <- 0
    for(i in 1:n) {             # loop over each row    // HL
        for(j in 1:m) {         # nest loop over each column // HL
            sum <- sum + x[i] * A[i,j] * y[j] # compute and add // HL
        }
    }
    return(sum)
}
