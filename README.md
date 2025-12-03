# rap

Audio processing utilities in R


# References

http://clas.mq.edu.au/speech/acoustics/frequency/spectral.html

http://clas.mq.edu.au/speech/synthesis/index.html

http://en.wikipedia.org/wiki/Linear_predictive_coding

http://www.ee.ic.ac.uk/pcheung/teaching/ee2_signals/ (Poles/filters)

https://maxwell.ict.griffith.edu.au/spl/publications/papers/book_sc_kkp.pdf

http://person2.sol.lu.se/SidneyWood/praate/whatform.html

https://www.sciencedirect.com/science/article/pii/016763939400058I

https://en.wikipedia.org/wiki/Z-transform (search for Fourier)

https://www.le.ac.uk/users/dsgp1/COURSES/THIRDMET/MYLECTURES/4XIDNTIFY.pdf

https://web.archive.org/web/20130313174105/http://tools.ietf.org/html/draft-vos-silk-02

http://soundlab.cs.princeton.edu/software/rt_lpc/

https://www.phon.ucl.ac.uk/courses/spsci/dsp/lpc.html (all-pole model == AR)

http://home.engineering.iastate.edu/~julied/classes/ee524/LectureNotes/l7b.pdf (z-transform)


May need to collect notes from more places, including git/isi/research/*/

- soundgen package: 

    - <https://cran.r-project.org/web/packages/soundgen/>
    - <https://link.springer.com/article/10.3758/s13428-018-1095-7>


# Notes

- Engineering lingo: AR == all-pole, MA = all-zero, ARMA =
  zero-pole. LTI=Linear Time Invariant

- pulse train noise may be better for voiced speech

- ar.yw() is a function of ACF. Can extract this part to be more
  efficient. Also, this algorithm is guaranteed to give "stable" AR
  estimates.

- One "good" corollary is that we can happily compress (and then
  decompress at destination) the ACF, and then use it to compute AR
  coefficients to synthesize. The only question is what happens if the
  ACF is not a "proper" ACF (in particular, maybe the implied Toeplitz
  covariance matrix is not p.d.)



