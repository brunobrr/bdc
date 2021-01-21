#' Flag missing coordinates
#'
#' @description
#' This function add a new column `.missing_xy` in the returned database
#'
#' @param data a data.frame with the default column names: "data",
#'      "lon", "lat"
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
  df<- data
  
  suppressWarnings({
    df <-
      df %>%
      dplyr::mutate_all(as.numeric)
  })
  
  df <-
    df %>%
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
    ) %>% 
    dplyr::select(.missing_xy)
  
  df <- dplyr::bind_cols(data,  df)
  
  message(
    paste(
      "\nbdc_flag_missing_xy:\nFlagged",
      sum(df$.missing_xy == FALSE),
      "records.\nOne column was added to the database.\n"))

    return(df)
}
