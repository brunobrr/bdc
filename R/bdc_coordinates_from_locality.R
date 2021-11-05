#' Identify records lacking or with invalid coordinates but containing locality
#' information
#'
#' This function Identifies records whose coordinates can potentially be
#' extracted from locality information.
#'
#' @param data data.frame. Containing geographical coordinates and the column
#' "locality'.
#' @param lat character string. The column name with latitude in decimal degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#' @param locality character string. The column name with locality information.
#' Default = "locality".
#'
#' @details According to DarwinCore terminology, locality refers to "the
#' specific description of the place" where an organism was recorded.
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

    if (!any(".coordinates_empty" == names(data))){
      
    data <-
      bdc_coordinates_empty(data = data,
                            lat = {{ lat }},
                            lon = {{ lon}})
    }

    if (!any(".coordinates_outOfRange" == names(data))){
      
    data <-
      bdc_coordinates_outOfRange(data = data,
                                lat = {{ lat }},
                                lon = {{ lon }})
    }

    })

    bdc_create_dir()

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
