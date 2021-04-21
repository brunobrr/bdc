
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BDC <a href='https://github.com/brunobrr/bdc'><img src='https://raw.githubusercontent.com/brunobrr/bdc/master/man/figures/logo.png' align="right" height="200" /></a>

## A comprehensive and straightforward workflow for standardizing, integrating, and cleaning biodiversity data

<!-- badges: start -->

[![R-CMD-check](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### Overview

Handle biodiversity data from several different sources is not an easy
task. This workflow was created to facilitate i) the standardization and
integration of heterogeneous datasets; and ii) flag, document, clean,
and correct errors in biodiversity datasets.

The workflow is composed of five core steps:

1.  [**Standardization** and
    **integration**](https://brunobrr.github.io/bdc/articles/integrate_datasets.html)
    of different datasets;
2.  [**Pre-filter**](https://brunobrr.github.io/bdc/articles/prefilter.html):
    flagging and removal of invalid or non-interpretable information,
    followed by data amendments (e.g., correct transposed coordinates
    and standardize country names);
3.  [**Taxonomy**](https://brunobrr.github.io/bdc/articles/taxonomy.html):
    cleaning, parsing, and standardization of scientific names against
    multiple taxonomic references. The workflow corrects spelling errors
    and converts nomenclatural synonyms to currently accepted names;
4.  [**Space**](https://brunobrr.github.io/bdc/articles/space.html):
    flagging of erroneous, suspicious, and low-precision geographic
    coordinates;
5.  [**Time**](https://brunobrr.github.io/bdc/articles/time.html):
    flagging and, whenever possible, correction of inconsistent
    collection date.

Aim to facilitate the interpretation and visualization of results,
reports and figures are created in each step of the workflow. Further,
standardized databases resulting from each step as well as data needing
further inspection are automatically saved in the “Output” folder.

### Installation

You can install the released version of “BDC” from
[github](https://github.com/brunobrr/bdc) with:

``` r
if (!require("remotes")) install.packages("remotes")
if (!require("bdc")) remotes::install_github("brunobrr/bdc")
```

### Package website

See BDC package website (<https://brunobrr.github.io/bdc/>) for detailed
explanation on each step of the workflow.

### Getting help

If you encounter a clear bug, please file an issue
[here](https://github.com/brunobrr/bdc/issues). For questions or
suggestion, please send us a email (ribeiro.brr@gmail.com).
