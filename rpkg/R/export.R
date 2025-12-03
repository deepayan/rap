


##' Scale an audio signal to be in standardized form between -1 and 1.
##'
##' This function scales an audio signal to lie between -1 and 1. The
##' maximum permissible value in the original signal is obtained from
##' the data rather than using a user-specified value; this is a
##' quantile of the abosolute values corresponding to probability
##' \code{qprob}, further multiplied by a factor of \code{(1 +
##' extend)}. Values whose magnitude fall outside this value are reset
##' to this bound before scaling, so that all values in the result lie
##' between -1 and 1.
##' @title Scale audio signal
##' @param x Integer or numeric vector representing a time series
##'     signal, typically audio.
##' @param qprob A probability, typically close to 1, to be used to to
##'     standardize the overall amplitude of the signal.
##' @param extend A factor by which the quantile obtained using
##'     \code{qprob} is adjusted. See details.
##' @return A numeric vector with values between -1 and 1
##' @author Deepayan Sarkar
scaleSignal <- function(x, qprob = 0.99, extend = 0.05)
{
    a <- abs(x)
    s <- sign(x)
    q <- quantile(a, qprob) * (1 + extend)
    a[a > q] <- q
    s * a / q
}



writeSignal <- function(x, file = "ouput.mp3", samp.rate = 44100, MAX = 2^31 - 1)
{
    ## x is a vector with numbers between 0 and 1. Anything outside
    ## needs to be truncated.
    x[x < -1] <- -1
    x[x >  1] <-  1
    write_audio_bin(as.integer(MAX * x),
                    pcm_channels = 1,
                    sample_rate = samp.rate,
                    output = file)
}


## unlike graphics, audio is not easily playable from R. This makes it
## difficult to work interactively (i.e. listen to results and tweak
## the code). To work around this, we can at least embed audio in HTML
## notebooks.


writeAudioBlock <- function(file, id = NULL)
{
    tt <-
        sprintf('<audio controls="" src="%s" %s style="display: block;"></audio>',
                file, if (is.null(id)) "" else sprintf('id="%s"', id))
    cat(tt, fill = TRUE)
}



htmlaudio <- function(filename)
{
    ## Prints HTML code for an <audio> element
    ## Assume that 'filename' is a valid audio file.

    type <- if (endsWith(filename, "mp3")) "audio/mpeg"
            else if (endsWith(filename, "ogg")) "audio/ogg"
            else if (endsWith(filename, "wav")) "audio/wav"
            else stop("unsupported file type")
    cat(sprintf("
 <audio controls>
  <source src='%s' type='%s'>
audio element not supported by browser.
</audio> 
", filename, type))
}

