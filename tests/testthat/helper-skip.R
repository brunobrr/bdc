# Skips with a concrete reason -------------------------------------------------
#
# Spatial functions read Natural Earth at `scale = "large"`, which needs both
# rnaturalearthdata and rnaturalearthhires (the latter is not on CRAN).

skip_if_no_natural_earth <- function() {
  testthat::skip_if_not_installed("rnaturalearthdata")
  testthat::skip_if_not_installed("rnaturalearthhires")
}

# bdc_clean_names() calls the external gnparser binary through rgnparser.
skip_if_no_gnparser <- function() {
  found <- tryCatch(
    {
      rgnparser::gn_version()
      TRUE
    },
    error = \(e) FALSE
  )
  testthat::skip_if_not(found, "gnparser is not installed")
}
