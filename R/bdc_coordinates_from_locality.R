#' Identify records missing or with invalid coordinates but with information on
#' locality
#'
#' This function Identifies records whose coordinates can potentially be
#' extracted from information on locality.
#'
#' @param data data.frame. Containing geographical coordinates and the column
#' "locality'.
#' @param lat character string. The column name with latitude. Coordinates must
#' be expressed in decimal degree and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude. Coordinates must be
#' expressed in decimal degree and in WGS84. Default = "decimalLongitude".
#' @param locality character string. The column name with information on
#' locality. Default = "locality".
#' 
#' @details According to DarwinCore terminology, locality refers to "the
#' specific description of the place" where a organism were recorded.
#' 
#' @return A data.frame containing records missing or with invalid coordinates
#' but with potentially useful locality information is saved in
#' Output/Check/01_coordinates_from_locality.csv.
#' 
#' @importFrom data.table fwrite
#' @importFrom dplyr filter select
#' @importFrom here here
#' @importFrom stringr str_detect str_squish
#' 
#' @export 
#'
#' @examples
#' \dontrun{
#' lat <- c(NA, NA, "")
#' lon <- c("", NA, NA)
#' x <- data.frame(lat = lat, lon = lon, locality =  c("Brazil", "Argentina", "Chile"))
#' bdc_coordinates_from_locality(data = x, lat = "lat", lon = "lon")
#' }
bdc_coordinates_from_locality <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           locality = "locality") {
    
    suppressMessages({
    col_names <- c(".coordinates_empty", ".coordinates_outOfRange")
    if (!all(col_names %in% names(data))){
    data <-
      bdc_coordinates_empty(data = data, 
                            lat = {{ lat }}, 
                            lon = {{ lon}})

    data <-
      bdc_coordinates_outOfRange(data = data, 
                                lat = {{ lat }}, 
                                lon = {{ lon }})
    }

    })
    
    df <-
      data %>%
      dplyr::mutate(locality == stringr::str_squish(.data[[locality]])) %>%
      dplyr::filter(
        locality != "" & !is.na(locality),
        stringr::str_detect(locality, "^(\\. )", negate = T),
        .coordinates_empty == FALSE |
          .coordinates_outOfRange == FALSE
      )
    
    
    df <-
      df %>%
      dplyr::select(-c(.coordinates_empty, .coordinates_outOfRange))
    
    save <- here::here("Output/Check/01_coordinates_from_locality.csv")
    df %>% data.table::fwrite(save)

    message(
      paste(
        "\nbdc_coordinates_from_locality",
        "\nFound",
        nrow(df),
        "records missing or with invalid coordinates but with potentially useful information on locality.\n",
        "\nCheck database in:",
        save
      )
    )

    return(df)
  }