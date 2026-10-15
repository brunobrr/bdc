# Temporary project root -------------------------------------------------------
#
# bdc reads and writes workflow outputs under `here::here("Output", ...)`.
# `here` resolves the project root once, when it is loaded, so during
# `devtools::test()` it points at the package checkout. These helpers give a
# test its own throwaway project instead.

#' Run the calling test inside a throwaway bdc project
#'
#' Creates a temporary directory with a `.here` marker, points
#' `here::here()` at it and moves the working directory into it (or into
#' `subdir`, created if needed, to mimic a knitted Rmd in a subfolder). The
#' directory, the working directory and the `here` binding are all restored
#' when `.local_envir` exits.
#'
#' @param subdir Optional path, relative to the project root, to use as the
#'   working directory.
#' @param .local_envir Environment whose exit triggers the cleanup.
#' @return The normalized path of the project root.
#' @noRd
local_temp_project <- function(subdir = NULL, .local_envir = parent.frame()) {
  root <- withr::local_tempdir(pattern = "bdc-project-", .local_envir = .local_envir)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  file.create(file.path(root, ".here"))

  testthat::local_mocked_bindings(
    here = function(...) {
      if (...length() == 0) root else file.path(root, ...)
    },
    .package = "here",
    .env = .local_envir
  )

  wd <- root
  if (!is.null(subdir)) {
    wd <- file.path(root, subdir)
    dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  }
  withr::local_dir(wd, .local_envir = .local_envir)

  root
}
