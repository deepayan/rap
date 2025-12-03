
## High-pass filter: shrink magnitudes of all frequencies towards 0 by
## fixed amount.


## fft(z, inverse = FALSE)
## mvfft(z, inverse = FALSE)


transform.fft <- function(delta = 0, modulus.only = TRUE)
{
    force(delta)
    if (modulus.only)
        function(x)
        {
            n <- nrow(x)
            X <- Mod(mvfft(x) / sqrt(n))
            X[] <- pmax(Mod(X) - delta, 0)
            Re(mvfft(X * sqrt(n), inverse = TRUE))
        }
    else
        function(x)
        {
            n <- nrow(x)
            X <- mvfft(x) / sqrt(n)
            X[] <- complex(modulus = pmax(Mod(X) - delta, 0),
                           argument = Arg(X))
            Re(mvfft(X * sqrt(n), inverse = TRUE))
        }
}

