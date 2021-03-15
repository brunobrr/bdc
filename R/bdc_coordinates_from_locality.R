#' Identify records missing coordinates but with information on locality
#'
#' Identify records whose coordinates can potentially be extracted from
#' information on locality.
#' 
#' @param data data.frame. Containing geographical coordinates and the column
#' "locality'.
#' @param locality character string. The column with information on where records
#' was collected. Default = "locality".
#' @param lat character string. The column name with latitude Default =
#' "decimalLatitude".
#' @param lon character string. The column name with longitude. Default =
#' "decimalLongitude".
#'
#' @return A data.frame containing records missing coordinates but with potentially useful locality information is saved in Output/Check/01_coordinates_from_locality.csv
#' @export 
#'
#' @examples
bdc_coordinates_from_locality <-
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

    save <- here::here("Output/Check/01_coordinates_from_locality.csv")
    df %>%
      data.table::fwrite(save)

    message(
      paste(
        "\nbdc_coordinates_from_locality",
        "\nFound",
        nrow(df),
        "records missing or with invalid coordinates but with potentially useful 
        information on locality.\n",
        "\nCheck database in:",
        save
      )
    )

    return(df)
  }