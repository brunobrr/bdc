#' Flag missing coordinates
#'
#' @description
#' This function add a new column `.invalid_xy` in the returned dataset
#'
#' @param data a data.frame with the default column names: "database_id",
#'      "scientificName", "decimalLongitude", "decimalLatitude"
#'
#' @importFrom dplyr mutate case_when
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_invalid_xy()
#' }
bdc_flag_missing_xy <- function(data, lon, lat) {
  suppressWarnings({
    data <-
      data %>%
      dplyr::mutate_all(as.numeric)
  })
  
  data <-
    data %>%
    dplyr::mutate(
      .missing_xy = dplyr::case_when(
        is.na(!!rlang::sym(lat)) | is.na(!!rlang::sym(lon)) ~ FALSE,
        # flag empty coordinates
        nzchar(!!rlang::sym(lat)) == FALSE |
          nzchar(!!rlang::sym(lon)) == FALSE ~ FALSE,
        # flag empty coordinates
        is.numeric(!!rlang::sym(lat)) == FALSE |
          is.numeric(!!rlang::sym(lon)) == FALSE ~ FALSE,
        # opposite cases are flagged as TRUE
        TRUE ~ TRUE
      )
    )
  
  message(paste(
    "Flagged",
    sum(data$.missing_xy == FALSE),
    "out",
    nrow(data),
    "records"
  ))
  
  return(data %>% pull(.missing_xy))
  

}
