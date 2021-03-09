#' Identify records missing geographic coordinates
#'
#' Flags records missing latitude or longitude coordinates.
#'
#' @param data data.frame. Containing geographical coordinates.
#' @param lat character string. The column with latitude Default =
#' "decimalLatitude".
#' @param lon character string. The column with longitude. Default =
#' "decimalLongitude".
#'
#' @details This test identifies records missing geographic coordinates (i.e.,
#' empty or not applicable [NA] longitude or latitude)
#'
#' @return A data.frame contain the column '.missing_xy'. Records that have
#' failed in the test are flagged as "FALSE".
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
#' bdc_missing_xy(data = x, lat = "decimalLatitude", lon = "decimalLongitude")
#' }
bdc_missing_xy <-
  function(data, lon = "decimalLongitude", lat = "decimalLatitude") {
    df <- data

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
        "\nbdc_missing_xy:\nFlagged",
        sum(df$.missing_xy == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(df)
  }
