#' Create directories for saving the outputs of the bdc workflow
#'
#' Creates directories for saving the outputs of BDC workflow, which including
#' databases, figures, reports, and databases that need to be checked.
#'
#' @importFrom fs dir_create
#' @importFrom here here
#'
#' @return None
#'
#' @export
#'
#' @details:
#' Check below the folder structure created by `bdc_create_dir()`.
#' ```
#' .
#' └── Output
#'     ├── Check
#'     ├── Figures
#'     ├── Intermediate
#'     └── Report
#' ```
#' @examples
#' \dontrun{
#' bdc_create_dir()
#' }
bdc_create_dir <- function(){
  fs::dir_create(here::here("Output/Check"))
  fs::dir_create(here::here("Output/Intermediate"))
  fs::dir_create(here::here("Output/Report"))
  fs::dir_create(here::here("Output/Figures"))
}
