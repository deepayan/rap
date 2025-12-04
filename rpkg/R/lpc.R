
## ACF and AR related functions (circa 2020)

## FIXME: replace hard-coded 1024 by argument.

generateAR <-
    function(arm, innov.fun = rnorm, ...,
             noise.sigma = 1,
             verbose = interactive())
        ## arm: matrix of AR coefficients
{
    ORDER <- nrow(arm)
    ## matplot(asinh(arm)[1:10, ], type = "l", lty = 1, col = "#00000022")
    N <- 1024 * (1 + ncol(arm))
    zz <- noise.sigma * innov.fun(N, ...)
    xx <- zz
    IND <- 1:ORDER
    for (i in 1025:(N-1))
    {
        j <- i %/% 1024
        rho <- arm[, j]
        xx[i] <- sum(rho * xx[i - IND]) + zz[i]
        if (!is.finite(xx[i])) xx[i] <- 0
        if (verbose && i %% 1000 == 0) {
            cat(sprintf("\r %d%%: %g              ",
                        round(100 * i / (N-1)), xx[i]))
            if (!is.finite(xx[i])) ## stop("Terminating.")
                xx[!is.finite(xx)] <- 0
        }
    }
    print(range(xx))
    xx[!is.finite(xx)] <- 0
    scaleSignal(xx, qprob = 0.99)
}


## Unfortunately, there is no easily available implementation that
## solves for AR coefficients given ACF, so we may need to write
## one. This basically involves solving a system of equations with a
## Toeplitz matrix. A crude solution is

acf2ar <- function(rho, lambda = 0)
    ## rho : rho_0, rho_1, ..., rho_(p-1), actually covariances
{
    p <- length(rho) - 1
    rho[1] <- rho[1] + lambda # ridge adjustment
    solve(toeplitz(rho[-(p+1)]), rho[-1])
}




## See signal::levinson()

levinson.ma <- function(acf, p = length(acf)-1L)
{
    ref <- numeric(p)
    g <- -acf[2] / acf[1]
    a <- g
    v <- (1 - g * g) * acf[1]
    ref[1] <- g
    ## str(list(g = g, a = a, v = v))
    for (t in 2:p) {
        g[] <- -(acf[t + 1] + a %*% acf[seq(t, 2, by = -1)]) / v
        a <- c((a + g * a[seq(t - 1, 1, -1)]), g)
        v[] <- v * (1 - g * g)
        ref[t] <- g
        ## str(list(g = g, a = a, v = v))
    }
    ## a <- c(1, a)
    ## return(list(a = a, v = v, ref = ref))
    ## a / max(abs(a))
    a
}


smallestEigenvalue <- function(rho, lambda = 0)
{
    p <- length(rho) - 1
    rho[1] <- rho[1] + lambda # ridge adjustment
    T <- toeplitz(rho[-(p+1)])
    e <- eigen(T, symmetric = TRUE, only.values = TRUE)$values
    sum(e <= 0)
}


## If 'r <- eigen(A)', and 'V <- r$vectors; lam <- r$values', then
##                          A = V Lmbd t(V)
## (up to numerical fuzz), where Lmbd ='diag(lam)'.


makePD <- function(rho, lambda = 0, eps = 0.00001)
{
    p <- length(rho) - 1
    rho[1] <- rho[1] + lambda # ridge adjustment
    T <- toeplitz(rho[-(p+1)])
    r <- eigen(T, symmetric = TRUE, only.values = FALSE)
    V <- r$vectors
    lam <- r$values
    lam[lam <= 0] <- eps
    A <- V %*% diag(lam) %*% t(V)
    max(abs(A - toeplitz(A[1,]))) ## ~ not really
    ## c(A[1,], rho[p+1])
}



## Let's at least try solving as non-Toeplitz

acf2ar.pd <- function(rho, lambda = 0, eps = 0.00001)
    ## rho : rho_0, rho_1, ..., rho_(p-1), actually covariances
{
    p <- length(rho) - 1
    rho[1] <- rho[1] + lambda # ridge adjustment
    T <- toeplitz(rho[-(p+1)])
    r <- eigen(T, symmetric = TRUE, only.values = FALSE)
    lam <- r$values
    if (any(lam <= 0))
    {
        V <- r$vectors
        lam[lam <= 0] <- eps
        T <- V %*% diag(lam) %*% t(V)
    }
    solve(T, rho[-1])
}




