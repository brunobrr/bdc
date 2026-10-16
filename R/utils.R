check_require_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: `install.packages(\"", pkg, "\")`"), call. = FALSE)
  }
  invisible(TRUE)
}

check_require_github <- function(pkg) {
  userpkg <- pkg
  pkg <- basename(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: remotes::install_github(\"", userpkg, "\")"), call. = FALSE)
  }
  invisible(TRUE)
}

#' Internal function. Check that rank and rank_name come together
#'
#' @param rank,rank_name See bdc_query_names_taxadb().
#' @return TRUE, invisibly; errors when only one of them is supplied.
#' @noRd
bdc_check_rank_args <- function(rank, rank_name) {
  if (is.null(rank) != is.null(rank_name)) {
    stop(
      "Please, provide both 'rank' and 'rank_name' arguments, or neither ",
      "(supplied: ", if (is.null(rank)) "'rank_name'" else "'rank'", ")",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Internal function. Path of the transposed-coordinates check file
#'
#' Written by bdc_coordinates_transposed() and read by bdc_create_report() and
#' bdc_create_figures(), always at the project root (#295).
#'
#' @return A file path.
#' @noRd
bdc_transposed_check_file <- function() {
  here::here("Output", "Check", "01_coordinates_transposed.csv")
}

has_table <- function(table = NULL, db = td_connect()) {
  if (is.null(db)) {
    return(FALSE)
  } else if (table %in% DBI::dbListTables(db)) {
    return(TRUE)
  } else {
    FALSE
  }
}

bdc_taxadb_dir <- function() {
  Sys.getenv("TAXADB_HOME", rappdirs::user_data_dir("taxadb"))
}

check_col <- function(data, col) {
  for (i in seq_along(col)) {
    if (!col[i] %in% colnames(data)) {
      stop("Column `", col[i], "` is not present in the data", call. = FALSE)
    }
  }
}
