


## Utilities to allow experimentation.

## Basic idea: Assume input signal is a numeric (usually integer) vector between -R and R.
## 
## - Scale to [-1, 1]
##
## - split() into overlapping windows (patch = 2048, overlap = 1024),
##   creating a patch x N matrix. Drop incomplete column at the end if
##   present
##
## - merge() back into a vector by recombining. Deal with overlaps by
##   taking weighted sums, with weights linearly varying from 0 to 1
##   to 0 for each column.
##
## The idea is that we would apply an arbitrary transformation
## function f : R^n -> R^n after splitting, and then merge to get a
## "reconstructed" signal. Trying different f allows
## experimentation. The weighted sum is important to avoid
## discontinuities in the reconstructed signal, which leads to ticking
## noise.

splitSignal <- function(x, patch = 2048, overlap = 1024)
{
    ## Things would be easier if we assume that patch = 2 * overlap,
    ## but let's keep it more general.
    n <- length(x)
    stopifnot(overlap < patch, n > patch)
    offset <- patch - overlap # difference between successive start positions
    N <- floor(n / offset)
    while ((N-1) * offset + patch > n) N <- N - 1
    ## n <- N * offset + patch # ignore rest
    P <- seq_len(patch)
    m <- sapply(1:N, function(i) x[(i-1) * offset + P])
    attr(m, "overlap") <- overlap
    m
}

mergeSignal <- function(m, overlap = attr(m, "overlap"))
{
    N <- ncol(m)
    patch <- nrow(m)
    offset <- patch - overlap
    n <- N * offset + patch
    x <- numeric(n)
    P <- seq_len(patch)
    wts <- if (overlap > patch / 2) # FIXME
               stop("overlap > patch / 2 not supported yet")
           else
               approx(x = c(1, overlap, patch - overlap + 1, patch),
                      y = c(0, 1, 1, 0), xout = P)$y
    m[] <- m * wts
    for (i in 1:N)
        x[(i-1) * offset + P] <-
            x[(i-1) * offset + P] + m[, i]
    x
}


## Apply vectorized function FUN on each column. Use loop = FALSE to
## indicate that FUN can do that by itself (i.e., FUN is
## 'matrix-ised'), and apply is not needed. This is supported by
## mvfft() for example.

transformSignal <- function(m, FUN, loop = TRUE)
{
    if (loop)
        apply(m, 2, FUN)
    else
        FUN(m)
}

