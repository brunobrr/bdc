---
editor_options: 
  markdown: 
    wrap: 72
---

## Sumbmitting a new version

Dear CRAN Maintainers,

I am delighted to submit the latest version of bdc for your consideration.

Please find the results of GitHub actions checks and rhub copied below. I am pleased to report that there were no errors detected during these tests.

Thank you for your time and consideration.
Best regards,
Bruno R. Ribeiro


## GITHUB ACTIONS CHECK

── R CMD check results ─────────────────────────────── bdc 1.1.5 ────
Duration: 4m 38s

❯ checking package dependencies ... NOTE
  Imports includes 21 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

❯ checking installed package size ... NOTE
    installed size is  6.4Mb
    sub-directories of 1Mb or more:
      doc       4.8Mb
      extdata   1.2Mb

❯ checking installed files from ‘inst/doc’ ... NOTE
  The following directories should probably not be installed:
    ‘images’
  
  Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
  or move the vignette sources from ‘inst/doc’ to ‘vignettes’.

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

## RHUB CHECK
── R CMD check results ────────────────────────────────────────── bdc 1.1.5 ────
Duration: 1m 59.9s

❯ checking files in ‘vignettes’ ... WARNING
  Files in the 'vignettes' directory but no files in 'inst/doc':
    ‘articles/integrate_datasets.Rmd’ ‘articles/prefilter.Rmd’
    ‘articles/space.Rmd’ ‘articles/taxonomy.Rmd’ ‘articles/time.Rmd’
    ‘help/installing_gnparser.Rmd’
    ‘images/map_summary_space_vignette.png’
    ‘images/prefilter_.coordinates_country_inconsistent_MAP.png’
    ‘images/prefilter_summary_all_tests_BAR.png’
    ‘images/space_.rou_BAR.png’ ‘images/space_.urb_MAP.png’
    ‘images/space_summary_all_tests_BAR.png’
    ‘images/time_.eventDate_empty_BAR.png’ ‘images/time_.summary_BAR.png’
    ‘images/time_summary_all_tests_BAR.png’ ‘images/time_year_BAR.png’

❯ checking package vignettes ... NOTE
  Package has ‘vignettes’ subdirectory but apparently no vignettes.
  Perhaps the ‘VignetteBuilder’ information is missing from the
  DESCRIPTION file?

0 errors ✔ | 1 warning ✖ | 1 note ✖