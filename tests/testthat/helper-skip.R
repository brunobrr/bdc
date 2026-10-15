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

# Tests that really query taxadb download its Parquet files from
# data.source.coop, which sometimes answers HTTP 5xx; a failed download can also
# leave a truncated file in the local cache. `taxadb_or_skip()` evaluates `expr`
# and turns those host failures into a skip that names the error. Any other
# error still fails. At the top level of a file, the skip covers the rest of the
# file.
taxadb_or_skip <- function(expr) {
  tryCatch(expr, error = function(e) {
    msgs <- character()
    cnd <- e
    while (!is.null(cnd)) {
      msgs <- c(msgs, conditionMessage(cnd))
      cnd <- cnd$parent
    }
    host_failure <- "HTTP (GET )?[Ee]rror|too small to be a Parquet file"
    hit <- grep(host_failure, unlist(strsplit(msgs, "\n")), value = TRUE)
    if (length(hit) == 0) stop(e)
    testthat::skip(paste("taxadb data host failed:", trimws(hit[[1]])))
  })
}
