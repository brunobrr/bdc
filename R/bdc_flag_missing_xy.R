#' Title Flag records missing latitude or longitude
#'
#' @param data. data.frame containing longitude and latitude
#' @param lon. character string. The column with longitude coordinates
#' @param lat.  character string. The column with latitude coordinates
#'
#' @return a logical vector with TRUE = valid records and FALSE = invalid records
#' @export
#'
#' @examples
#' @details Flag non-numeric latitude or longitude (e.g. NA, "NA", ",", "", "Brazil")
bdc_flag_missing_xy <- function(data, lon = "decimalLongitude", lat = "decimalLatitude"){
  lo <- suppressWarnings({ purrr::map_lgl(data[[lon]], 
                                          function(i) i == as.numeric(i)) }) 
  lo <- ifelse(is.na(lo), FALSE, lo)
  
  la <- suppressWarnings({ purrr::map_lgl(data[[lat]], 
                                          function(i) i == as.numeric(i)) }) 
  la <- ifelse(is.na(la), FALSE, la)
  
  .missing_xy <- ifelse(la == TRUE & lo == TRUE, TRUE, FALSE)
  
  return(.missing_xy)
}