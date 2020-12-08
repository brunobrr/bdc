
#' Title Flag records missing coordinates
#'
#' @param data. data.frame containing longitude and latitude
#' @param lon. character string. The column with longitude coordinates
#' @param lat.  character string. The column with latitude coordinates
#'
#' @return
#' @export
#'
#' @examples
bdc_flag_missing_xy <- function(data, lon = "decimalLongitude", lat = "decimalLatitude"){
  lo <- suppressWarnings({ purrr::map_lgl(data[[lon]], 
                                          function(i) i == as.numeric(i)) }) 
  lo <- ifelse(is.na(lo), FALSE, lo)
  
  la <- suppressWarnings({ purrr::map_lgl(data[[lat]], 
                                          function(i) i == as.numeric(i)) }) 
  la <- ifelse(is.na(la), FALSE, la)
  
  .missing_xy <- ifelse(la == TRUE & lo == TRUE, TRUE, FALSE)
  
  data$.missing_xy <- .missing_xy
  
  return(data)
}
