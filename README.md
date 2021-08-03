
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BDC <a href='https://github.com/brunobrr/bdc'><img src="https://raw.githubusercontent.com/brunobrr/bdc/master/man/figures/logo.png" align="right" height="200"/></a>

## bdc: An R toolkit for standardizing, integrating, and cleaning biodiversity data

<!-- badges: start -->

[![R-CMD-check](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brunobrr/bdc/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

### Overview

Handle biodiversity data from several different sources is not an easy
task. Here, we present the Biodiversity Data Cleaning (bdc), an R
package to address quality issues and improve the fitness-for-use of a
dataset. bdc contains functions to harmonize and integrate data from
different sources following common standards and protocols and
implements various tests and tools to flag, document, clean, and correct
taxonomic, spatial, and temporal data.

The bdc package is organized in thematic modules related to different
biodiversity dimensions, including:

1.  [**Standardization** and
    **integration**](https://brunobrr.github.io/bdc/articles/00_integrate_datasets.html)
    of different datasets;
2.  [**Pre-filter**](https://brunobrr.github.io/bdc/articles/01_☺prefilter.html):
    flagging and removal of invalid or non-interpretable information,
    followed by data amendments (e.g., correct transposed coordinates
    and standardize country names);
3.  [**Taxonomy**](https://brunobrr.github.io/bdc/articles/02_taxonomy.html):
    cleaning, parsing, and standardization of scientific names against
    multiple taxonomic references. The workflow corrects spelling errors
    and converts nomenclatural synonyms to currently accepted names;
4.  [**Space**](https://brunobrr.github.io/bdc/articles/03_space.html):
    flagging of erroneous, suspicious, and low-precision geographic
    coordinates;
5.  [**Time**](https://brunobrr.github.io/bdc/articles/04_time.html):
    flagging and, whenever possible, correction of inconsistent
    collection date.

Aim to facilitate the documentation, visualization, and interpretation
of results of data quality teststhe package contains functions for
documenting the results of the data-cleaning tests, including functions
for saving i) records needing further inspection, ii) databases
containing the results of each step, iii) figures, and iv) data-quality
reports.

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
