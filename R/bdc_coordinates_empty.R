#' Identify records with empty geographic coordinates
#'
#' This function flags records missing latitude or longitude coordinates.
#'
#' @family {prefilter}
#' @param data data.frame. Containing geographical coordinates.
#' @param lat character string. The column name with latitude in decimal degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#'
#' @details This test identifies records missing geographic coordinates (i.e.,
#' empty or not applicable [NA] longitude or latitude)
#'
#' @return A data.frame containing the column ".coordinates_empty". Compliant
#' (TRUE) if 'lat' and 'lon' are not empty; otherwise "FALSE".
#'
#' @importFrom dplyr mutate_all mutate case_when select bind_cols
#' @importFrom rlang sym
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' decimalLatitude <- c(19.9358, -13.016667, NA, "")
#' decimalLongitude <- c(-40.6003, -39.6, -20.5243, NA)
#' x <- data.frame(decimalLatitude, decimalLongitude)
#' 
#' bdc_coordinates_empty(
#' data = x, 
#' lat = "decimalLatitude", 
#' lon = "decimalLongitude")
#' }
bdc_coordinates_empty <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude") {
    df <- data
    
    suppressWarnings({
      df <-
        df %>%
        dplyr::mutate_all(as.numeric)
    })
    
    df <-
      df %>%
      dplyr::mutate(
        .coordinates_empty = dplyr::case_when(
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
      dplyr::select(.coordinates_empty)
    
    df <- dplyr::bind_cols(data, df)
    
    message(
      paste(
        "\nbdc_coordinates_empty:\nFlagged",
        sum(df$.coordinates_empty == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )
    
    return(df)
  }
