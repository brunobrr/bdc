#' Create a map of points using ggplot2
#'
#' Creates a map of points using ggplot2 useful for inspecting the results of
#' tests implemented in the bdc workflow.
#'
#' @param data data.frame. Containing longitude and latitude
#' @param lon character string. The column with the longitude coordinates
#' @param lat character string. The column with the latitude coordinates
#' @param col_to_map character string. Defining the column or color used to map.
#' Can be a color name (e.g "red") the the name of a column of data. Default =
#' "blue"
#' @param size numeric. The size of the points.
#' 
#' @details Only records with valid coordinates can be plotted. Records missing
#' or with invalid coordinates are removed prior creating the map.
#' 
#' @importFrom ggplot2 borders theme_void theme element_blank ggplot labs
#' geom_point aes coord_quickmap scale_color_manual
#' 
#' @export
#' 
#' @examples
#' \dontrun{
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
#'   size = 1)
#' }
bdc_quickmap <- function(data, lat, lon, col_to_map = NULL, size = size) {

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

  if (all(col_to_map %in% names(data))) {
    our_map <-
      data %>%
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
      ggplot2::coord_quickmap()+
      ggplot2::scale_color_manual(values = c("red", "blue"))
    } else {
    our_map <-
      data %>%
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
        col = {{col_to_map}}, # Map the color
        alpha = 1,
        size = size
      ) +
      our_theme +
      ggplot2::coord_quickmap()
    }
  return(our_map)
}
