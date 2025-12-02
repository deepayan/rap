makeSinCurve <-
    function(duration = 1,        # seconds
             freq = 261.63,       # Hz (per second)
             samp.rate = 44100,   # Hz (per second)
             amplitude = 1,       # 1 is maximum
             offset = 0)          # seconds
{
    tt <- seq(0, duration, length.out = duration * samp.rate) + offset
    amplitude * sin(2 * pi * freq * tt)
}

