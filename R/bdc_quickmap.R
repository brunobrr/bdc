#' Create a map of points using ggplot2
#'
#' Creates a map of points using ggplot2 useful for inspecting the results of
#' tests implemented in the bdc package.
#'
#' @param data data.frame. Containing geographical coordinates. Coordinates must
#' be expressed in decimal degree and in WGS84.
#' @param lat character string. The column name with latitude. Coordinates must
#' be expressed in decimal degree and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude. Coordinates must be
#' expressed in decimal degree and in WGS84. Default = "decimalLongitude".
#' @param col_to_map character string. Defining the column or color used to map.
#' It can be a color name (e.g., "red") or the name of a column of data. Default
#' = "blue"
#' @param size numeric. The size of the points.
#'
#' @details Only records with valid coordinates can be plotted. Records missing
#' or containing invalid coordinates are removed prior creating the map.
#'
#' @return A map of points created using ggplot2.
#' 
#' @importFrom ggplot2 theme_void theme element_blank ggplot labs aes
#' coord_quickmap scale_color_manual
#' @importFrom dplyr mutate filter select
#' @export
#'
#' @examples
#' \donttest{
#' decimalLatitude <- c(19.9358, -13.016667, -19.935800)
#' decimalLongitude <- c(-40.6003, -39.6, -40.60030)
#' .coordinates_out_country <- c(FALSE, TRUE, TRUE)
#' x <- data.frame(decimalLatitude, decimalLongitude, .coordinates_out_country)
#'
#' bdc_quickmap(
#'   data = x,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude",
#'   col_to_map = ".coordinates_out_country",
#'   size = 1
#' )
#' }
bdc_quickmap <- function(data, lat = "decimalLatitude", lon = "decimalLongitude", col_to_map = "red", size = 1) {
  .data <- .coordinates_empty <- .coordinates_outOfRange <- z <- NULL

  check_require_cran("ggplot2")

  world_borders <-
    ggplot2::borders(
      database = "world",
      fill = "gray75",
      colour = "gray88",
    )

  our_theme <-
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )

  data <-
    data %>%
    dplyr::mutate(
      decimalLatitude = as.numeric(.data[[lat]]),
      decimalLongitude = as.numeric(.data[[lon]])
    )

  # identifying empty or out-of-range coordinates
  suppressMessages({
    if (!all(c(".coordinates_empty", ".coordinates_outOfRange") %in%
      names(data))) {
      data_raw <-
        bdc_coordinates_empty(
          data = data,
          lat = lat,
          lon = lon
        )

      data_raw <-
        bdc_coordinates_outOfRange(
          data = data_raw,
          lat = lat,
          lon = lon
        )

      df <-
        data_raw %>%
        dplyr::filter(.coordinates_empty == T & .coordinates_outOfRange == T) %>%
        dplyr::select(-c(.coordinates_empty, .coordinates_outOfRange))
    } else {
      df <-
        data %>%
        dplyr::filter(.coordinates_empty == T & .coordinates_outOfRange == T) %>%
        dplyr::select(-c(.coordinates_empty, .coordinates_outOfRange))
    }
  })


  if (all(col_to_map %in% names(data))) {
    # lon <- df[, lon] 
    # lat <- df[, lat] 
    # col_to_map <- df[, col_to_map]
    our_map <-
      df %>%
      ggplot2::ggplot() +
      world_borders +
      # theme_bw() +
      ggplot2::labs(
        x = "Longitude",
        y = "Latitude"
        # title = paste("Based on ", n_nrow_data, "points")
      ) +
      ggplot2::geom_point(ggplot2::aes(
        x = .data[[lon]],
        y = .data[[lat]],
        col = .data[[col_to_map]], # Map the column
      ),
      alpha = 1,
      size = size
      ) +
      our_theme +
      ggplot2::coord_quickmap() +
      ggplot2::scale_color_manual(values = c("red", "blue"))
  } else {
    # lon <- df[, lon] 
    # lat <- df[, lat] 
    our_map <-
      df %>%
      ggplot2::ggplot() +
      world_borders +
      # theme_bw() +
      ggplot2::labs(
        x = "Longitude",
        y = "Latitude"
        # title = paste("Based on ", n_nrow_data, "points")
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = .data[[lon]],
          y = .data[[lat]]
        ),
        col = col_to_map, # Map the color
        alpha = 1,
        size = size
      ) +
      our_theme +
      ggplot2::coord_quickmap()
  }
  return(our_map)
}
