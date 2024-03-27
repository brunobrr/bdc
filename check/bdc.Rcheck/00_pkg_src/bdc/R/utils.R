check_require_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: `install.packages(\"", pkg, "\")`"), call. = FALSE)
  } else {
    require(pkg, character.only = TRUE)
  }
}

check_require_github <- function(pkg) {
  userpkg <- pkg
  pkg <- basename(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: remotes::install_github(\"", userpkg, "\")"), call. = FALSE)
  } else {
    require(pkg, character.only = TRUE)
  }
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
