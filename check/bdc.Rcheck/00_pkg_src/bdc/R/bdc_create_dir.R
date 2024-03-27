#' Internal function. Create directories for saving the outputs of the bdc package
#'
#' Creates directories for saving the outputs of bdc package, namely
#' checks, figures, reports, and databases.
#'
#' @importFrom fs dir_create
#' @importFrom here here
#'
#' @return None
#'
#' @details:
#' Function used to created four folder for saving results of some functions.
#' @noRd

#' @examples
#' \dontrun{
#' bdc_create_dir()
#' }
bdc_create_dir <- function() {
  fs::dir_create(here::here("Output/Check"))
  fs::dir_create(here::here("Output/Intermediate"))
  fs::dir_create(here::here("Output/Report"))
  fs::dir_create(here::here("Output/Figures"))
}
