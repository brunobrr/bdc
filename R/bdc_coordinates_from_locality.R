#' Identify records lacking or with invalid coordinates but containing locality
#' information
#'
#' This function Identifies records whose coordinates can potentially be
#' extracted from locality information.
#'
#' @family prefilter
#' @param data data.frame. Containing geographical coordinates and the column
#' "locality'.
#' @param lat character string. The column name with latitude in decimal degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#' @param locality character string. The column name with locality information.
#' Default = "locality".
#' @param save_outputs logical. Should a table containing transposed coordinates
#' saved for further inspection? Default = FALSE.
#'
#' @details According to DarwinCore terminology, locality refers to "the
#' specific description of the place" where an organism was recorded.
#'
#' @return A data.frame containing records missing or with invalid coordinates
#'   but with potentially useful locality information. When save_outputs = FALSE
#'   the data.frame is saved in Output/Check/01_coordinates_from_locality.csv
#'
#' @importFrom readr write_csv
#' @importFrom dplyr filter select
#' @importFrom here here
#' @importFrom stringr str_detect str_squish
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   lat = c(NA, NA, ""), 
#'   lon = c("", NA, NA), 
#'   locality = c("PARAGUAY: ALTO PARAGUAY: CO.; 64KM W PUERTO SASTRE", 
#'                "Parque Estadual da Serra de Caldas Novas, Goias, Brazil", 
#'                "Parque Nacional Iguazu"))
#' 
#' bdc_coordinates_from_locality(
#' data = x, 
#' lat = "lat", 
#' lon = "lon", 
#' locality = "locality", 
#' save_outputs = FALSE)
#' 
bdc_coordinates_from_locality <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           locality = "locality",
           save_outputs = FALSE) {
    .data <- .coordinates_empty <- .coordinates_outOfRange <- NULL

    if (!locality %in% colnames(data)) {
      stop(
        "Locality column was not found in your database")
    }
    
    suppressMessages({
      if (!any(".coordinates_empty" == names(data))) {
        data <-
          bdc_coordinates_empty(
            data = data,
            lat = {{ lat }},
            lon = {{ lon }}
          )
      }

      if (!any(".coordinates_outOfRange" == names(data))) {
        data <-
          bdc_coordinates_outOfRange(
            data = data,
            lat = {{ lat }},
            lon = {{ lon }}
          )
      }
    })

    df <-
      data %>%
      dplyr::mutate(locality = stringr::str_squish(locality)) %>%
      dplyr::filter(
        locality != "" & !is.na(locality),
        stringr::str_detect(locality, "^(\\. )", negate = T),
        .coordinates_empty == FALSE |
          .coordinates_outOfRange == FALSE
      )

    df <-
      df %>%
      dplyr::select(-all_of(c(.coordinates_empty, .coordinates_outOfRange)))

    if (save_outputs) {
      bdc_create_dir()

      df %>% readr::write_csv(
        here::here("Output/Check/01_coordinates_from_locality.csv"))
      
      message(
        paste(
          "\nbdc_coordinates_from_locality",
          "\nFound",
          nrow(df),
          "records missing or with invalid coordinates but with potentially useful information on locality.\n",
          "\nCheck database in:",
          "Output/Check/01_coordinates_from_locality.csv"
        )
      )
    }

    message(
      paste(
        "\nbdc_coordinates_from_locality",
        "\nFound",
        nrow(df),
        "records missing or with invalid coordinates but with potentially useful information on locality.\n"
      )
    )

    return(df)
  }
