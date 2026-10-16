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

#' Expect the package checkout's own Output/Check file to stay untouched
#'
#' Records the modification time of `Output/Check/<file>` in the package
#' checkout and, when `.local_envir` exits, expects it unchanged: a
#' `save_outputs = TRUE` call must write into the test's own project, never
#' into the checkout. Call it before `local_temp_project()`, which moves the
#' working directory.
#'
#' @param file File name under `Output/Check`.
#' @param .local_envir Environment whose exit triggers the check.
#' @noRd
local_package_check_untouched <- function(file, .local_envir = parent.frame()) {
  pkg_file <- file.path(
    testthat::test_path("..", ".."), "Output", "Check", file
  )
  pkg_file <- normalizePath(pkg_file, winslash = "/", mustWork = FALSE)
  before <- file.info(pkg_file)$mtime
  withr::defer(
    expect_identical(
      file.info(pkg_file)$mtime, before,
      label = paste0("package checkout's Output/Check/", file, " mtime")
    ),
    envir = .local_envir
  )
  invisible(pkg_file)
}

#' Expect code to write nothing into the project
#'
#' Evaluates `code` inside a throwaway project (see `local_temp_project()`)
#' and expects the project to hold nothing but its `.here` marker afterwards.
#'
#' @param code Code to evaluate, e.g. a call with `save_outputs = FALSE`.
#' @return The value of `code`, invisibly.
#' @noRd
expect_writes_nothing <- function(code) {
  root <- local_temp_project()
  value <- code
  expect_identical(
    list.files(root, recursive = TRUE, all.files = TRUE),
    ".here",
    label = "files in the project after the call"
  )
  invisible(value)
}
