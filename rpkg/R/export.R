
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

writeAudioBlock <- function(file, id = NULL)
{
    tt <-
        sprintf('<audio controls="" src="%s" %s style="display: block;"></audio>',
                file, if (is.null(id)) "" else sprintf('id="%s"', id))
    cat(tt, fill = TRUE)
}
