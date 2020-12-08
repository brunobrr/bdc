#' Flag invalid coordinates
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
bdc_flag_invalid_xy <- function(data, long, lat) {

  suppressWarnings({
    data <-
      data %>%
      dplyr::mutate_all(as.numeric)
  })

  data <-
    data %>%
    dplyr::mutate(
      .invalid_xy = dplyr::case_when(
        is.na(!!rlang::sym(lat)) | is.na(!!rlang::sym(long)) ~ TRUE,
        # flag empty coordinates
        nzchar(!!rlang::sym(lat)) == FALSE | nzchar(!!rlang::sym(long)) == FALSE ~ TRUE,
        # flag empty coordinates
        is.numeric(!!rlang::sym(lat)) == FALSE | is.numeric(!!rlang::sym(long)) == FALSE ~ TRUE,
        # opposite cases are flagged as FALSE
        TRUE ~ FALSE
      )
    )

  return(data)

}
