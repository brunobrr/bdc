---
editor_options: 
  markdown: 
    wrap: 72
---
## Resubmission

Dear CRAN Maintainers, Dear Dr. Julia Haider,

We are thankful for your comments and suggestion. 

We are pleased to resubmit the bdc package to CRAN. In this new version we fix all issues, specifically:

* Add one paragraph describing in more detail the package;

* Add \value to bdc_quickmap and bdc_standardize_datasets functions;

* Change, whenever, possible, \dontrun{} to \donttest{} and unwrap the examples that can be executed in <5 seconds

* Avoid functions to create folder/directory in the user's filespace.

* The manuscript describing the methods of the package is under review. We will add a reference in the DESCRIPTION when the manuscript is accepted for publication.

Below we copy the results of hrub and GitHub actions checks. There were no ERRORs or WARNINGs. 

Cheers,
Bruno R. Ribeiro

## HRUB CHECK

0 errors √ | 0 warnings √ | 3 notes x

-   checking CRAN incoming feasibility ... NOTE Maintainer: 'Bruno
    Ribeiro
    [ribeiro.brr\@gmail.com](mailto:ribeiro.brr@gmail.com){.email}'

New submission

-  checking installed package size ... NOTE
   installed size is  5.7Mb
   sub-directories of 1Mb or more:
     doc       4.3Mb
     extdata   1.1Mb

-   checking for detritus in the temp directory ... NOTE Found the
    following files/directories: 'lastMiKTeXException'

## GITHUB ACTIONS CHECK

-- R CMD check results ------------------------------------------ bdc 1.0.0 ----
Duration: 4m 1.7s

0 errors v | 0 warnings v | 0 notes v