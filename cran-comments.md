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

── R CMD check results ─── bdc 1.1.6 ────
Duration: 3m 24.6s

❯ checking R code for possible problems ... NOTE
  bdc_create_figures : create_barplot_all_tests: no visible binding for
    global variable ‘flagged’
  bdc_create_figures : create_barplot_all_tests: no visible binding for
    global variable ‘test_name’
  Undefined global functions or variables:
    flagged test_name

0 errors ✔ | 0 warnings ✔ | 1 note ✖