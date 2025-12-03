
## Time-domain smoothing by Ridge-like penalizing of either input
## signal, or gradients, or generally some filter. Can be generalized
## later to allow robust (non-L2) loss as well as non-L2 penalty.

## Use Matrix package for sparse Cholesky etc.


## Solve Ax=b. If A remains same across calls, the Cholesky
## decomposition will be reused.

solveAb <- function(A, b)
{
    C <- try(Cholesky(A, perm = TRUE, super = TRUE), silent = TRUE)
    if (inherits(C, "try-error"))
        C <- Cholesky(A, perm = TRUE, super = FALSE)
    solve(C, b)
}


## unsymmetric centered toeplitz. If x has odd length, middle element
## becomes diagonal.

ctoeplitz <- function(x, size)
    ## x contains part corresponding to convolution
    ## size is dimension of resulting Toeplitz matrix
{
    n <- length(x)
    shift <- floor((n-1)/2)
    ## create matrix of dimension size+shift, then drop rows/columns
    trow <- toeplitz(c(x, rep(0, size + shift - n)))
    trow[lower.tri(trow)] <- 0
    if (shift == 0)
        Matrix(trow, sparse = TRUE)
    else
        Matrix(trow[-(size+seq_len(shift)), -seq_len(shift)], sparse = TRUE)
}

constructA <- function(n, filter = 1, lambda = 0.01)
{
    A <- Diagonal(n) + 2 * lambda * crossprod(ctoeplitz(filter, n))
    ## print(A[1:5, 1:5])
    A
}

transform.ridge <- function(filter = 1, lambda = 0.01)
{
    force(filter)
    force(lambda)
    function(x)
    {
        n <- nrow(x)
        A <- constructA(n, filter = filter, lambda = lambda)
        solveAb(A, x)
    }
}

