check_require_cran <- function(pkg) {
  full_pkgname <- pkg
  pkgname <- basename(full_pkgname)
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: install.packages(\"", pkg, "\")"), call. = FALSE)
  }
}

check_require_github <- function(pkg) {
  full_pkgname <- pkg
  pkgname <- basename(full_pkgname)
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: remotes::install_github(\"", pkg, "\")"), call. = FALSE)
  }
}
