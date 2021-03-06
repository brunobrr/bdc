#' Identify records missing coordinates but with information on locality
#'
#' Identify records whose coordinates can potentially be extracted from
#' information on locality 
#' 
#' @param data data.frame. Containing geographical coordinates and the column
#' "locality'.
#' @param locality chracter string. The column with information on where records
#' was collected.
#' @param lat character string. The column with latitude Default =
#' "decimalLatitude".
#' @param lon character string. The column with longitude. Default =
#' "decimalLongitude".
#'
#' @return
#' @export
#'
#' @examples
bdc_xy_from_locality <-
  function(data,
           locality = "locality",
           lon = "decimalLongitude",
           lat = "decimalLatitude") {
    df <-
      data %>%
      dplyr::filter(
        .invalid_xy == FALSE | .missing_xy == FALSE,
        .data[[locality]] != "" &
          !is.na(.data[[locality]])
      )

    save <- here::here("Output/Check/01_xy_from_locality.csv")
    df %>%
      data.table::fwrite(save)

    message(
      paste(
        "\nbdc_xy_from_locality",
        "\nFound",
        nrow(df),
        "records missing or with invalid xy but with potentially useful information on locality.\n",
        "\nCheck database in:",
        save
      )
    )

    return(df)
  }