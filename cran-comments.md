---
editor_options: 
  markdown: 
    wrap: 72
---

Dear CRAN Maintainers,

We are pleased to submit the bdc package to CRAN. Below we copy the
results of hrub and GitHub actions checks and added the justify one
note.

Cheers,

Bruno R. Ribeiro

## HRUB CHECK

0 errors √ \| 0 warnings √ \| 4 notes x

-   checking CRAN incoming feasibility ... NOTE Maintainer: 'Bruno
    Ribeiro
    [ribeiro.brr\@gmail.com](mailto:ribeiro.brr@gmail.com){.email}'

New submission

-   checking installed package size ... NOTE installed size is 6.8Mb
    sub-directories of 1Mb or more: doc 4.3Mb extdata 2.2Mb

-   checking for unstated dependencies in vignettes ... NOTE 'library'
    or 'require' call not declared from: 'rnaturalearthhires'

**COMMENT:** *This package is essential for our operation,
unfortunately, it is not available on CRAN, only on Github. We've done
our best to exclude all non-cran dependencies. This one, however, was
kept because it is important for building the vignettes.*

-   checking for detritus in the temp directory ... NOTE Found the
    following files/directories: 'lastMiKTeXException'

'''

## GITHUB ACTIONS CHECK

'-- R CMD check results ------------------------------------------ bdc
1.0.0 ---- Duration: 3m 44.5s

> checking for unstated dependencies in vignettes ... NOTE 'library' or
> 'require' call not declared from: 'rnaturalearthhires'

0 errors v \| 0 warnings v \| 1 note x '''
