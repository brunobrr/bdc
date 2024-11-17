---
editor_options: 
  markdown: 
    wrap: 72
---

## Sumbmitting a new version

Dear CRAN Maintainers,

I am delighted to submit the latest version of bdc for your consideration.

Please find the results of GitHub actions checks copied below. I am pleased to report that there were no errors detected during these tests.

I also want to inform that bdc_install_gnparser are only a internal function not available to the general public.

Thank you for your time and consideration.
Best regards,
Bruno R. Ribeiro


## GITHUB ACTIONS CHECK

── R CMD check results ─────────────────────────────────── bdc 1.1.5 ────
Duration: 4m 40.6s

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

❯ checking package dependencies ... NOTE
  Imports includes 23 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable. Move as many as possible to Suggests and
  use conditionally.

❯ checking R code for possible problems ... [14s/15s] NOTE
  bdc_install_gnparser: no visible global function definition for
    ‘download.file’
  bdc_install_gnparser: no visible global function definition for ‘unzip’
  bdc_install_gnparser: no visible global function definition for ‘untar’
  bdc_standardize_country: no visible binding for global variable ‘.’
  Undefined global functions or variables:
    . download.file untar unzip
  Consider adding
    importFrom("utils", "download.file", "untar", "unzip")
  to your NAMESPACE file.

**COMMENTS**: This is a internal function, not available to the general public.

0 errors ✔ | 1 warning ✖ | 2 notes ✖
