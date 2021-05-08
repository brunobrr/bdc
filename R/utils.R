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
