#' Title: Create a map of points using ggplot2
#'
#' @param data data.frame containing longitude and latitude
#' @param lon character string. The column with the longitude coordinates
#' @param lat character string. The column with the latitude coordinates
#' @param col_to_map character string. Defining the column or color used to map. Can be a color name (e.g "red") the the name of a column of data. Dafault = "blue"
#' @param size numeric. The size of the points

bdc_quickmap <- function(data, lon, lat, col_to_map = NULL, size = size) {
  
  # n_nrow_data <- format(x = nrow(data), big.mark = ",")
  # print(paste("Based on", n_nrow_data, "points"))
  # 
  world_borders <-
    borders(
      database = "world",
      fill = "white",
      colour = "grey90",
    )
  
  our_theme <-
    ggplot2::theme(
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  if (col_to_map %in% names(data)) {
    our_map <-
      data %>%
      ggplot() +
      world_borders +
      theme_bw() +
      labs(
        x = "Longitude",
        y = "Latitude"
        # title = paste("Based on ", n_nrow_data, "points")
      ) +
      geom_point(aes(
        x = .data[[lon]],
        y = .data[[lat]],
        col = .data[[col_to_map]], # Map the column
      ),
      alpha = 0.5,
      size = size
      ) +
      our_theme
  } else {
    our_map <-
      data %>%
      ggplot() +
      world_borders +
      theme_bw() +
      labs(
        x = "Longitude",
        y = "Latitude"
        # title = paste("Based on ", n_nrow_data, "points")
      ) +
      geom_point(
        aes(
          x = .data[[lon]],
          y = .data[[lat]]
          # Map the color
        ),
        col = col_to_map,
        alpha = 0.5,
        size = size
      ) +
      our_theme
  }
  return(our_map)
}
