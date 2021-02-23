#' Title
#'
#' @param data 
#' @param locality 
#' @param lon 
#' @param lat 
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