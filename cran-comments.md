---
editor_options: 
  markdown: 
    wrap: 72
---

## Sumbmitting a new version

Dear CRAN Maintainers,

I am delighted to submit the latest version of bdc for your consideration.

Please find the results of hrub and GitHub actions checks copied below. I am pleased to report that there were no errors or warnings detected during these tests.

I also want to inform that have thoroughly reviewed any NOTES that arose and confirmed that all the URLs are functional.

Thank you for your time and consideration.
Best regards,
Bruno R. Ribeiro


## HRUB CHECK

0 errors √ \| 0 warnings √ \| 3 notes x

-   Maintainer: 'Bruno Ribeiro \<ribeiro.brr\@gmail.com\>' New
    submission

-  Found the following (possibly) invalid URLs:
   URL: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152
    From: inst/doc/space.html
    Status: 403
    Message: Forbidden

-   URL: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13440
    From: inst/doc/taxonomy.html
    Status: 403
    Message: Forbidden
    
- checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    doc       3.6Mb
    extdata   1.2Mb
**COMMENTS**: we double-checked this and the urls are working

## GITHUB ACTIONS CHECK

0 errors ✔ | 0 warnings ✔ | 1 note ✖


-   checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      doc       3.5Mb
      extdata   1.2Mb
