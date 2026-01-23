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

── R CMD check results ───────────────────────────────────────── bdc 1.1.6 ────
Duration: 3m 6.1s

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

❯ checking R code for possible problems ... NOTE
  bdc_create_figures : create_barplot_all_tests: no visible binding for
    global variable ‘flagged’
  bdc_create_figures : create_barplot_all_tests: no visible binding for
    global variable ‘test_name’
  Undefined global functions or variables:
    flagged test_name

❯ checking package vignettes ... NOTE
  Package has ‘vignettes’ subdirectory but apparently no vignettes.
  Perhaps the ‘VignetteBuilder’ information is missing from the
  DESCRIPTION file?

0 errors ✔ | 1 warning ✖ | 2 notes ✖