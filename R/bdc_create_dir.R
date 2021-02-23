#' Title
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
