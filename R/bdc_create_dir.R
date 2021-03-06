#' Creates directories for saving ouptups of the bdc workflow
#'
#' Creates directories for saving the outputs of bdc workflow, which including 
#' databases, figures, reports, and databases which need to be checked.
#' 
#' @return
#' @export
#'
#' @examples
bdc_create_dir <- function(){
  fs::dir_create(here::here("Output/Check"))
  fs::dir_create(here::here("Output/Intermediate"))
  fs::dir_create(here::here("Output/Report"))
  fs::dir_create(here::here("Output/Figures"))
}
