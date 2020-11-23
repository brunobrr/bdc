#' Title: Create a map of points using ggplot2
#'
#' @param data 
#' @param lon 
#' @param lat 
#'
#' @return
#' @export
#'
#' @examples
bdc_quickmap <- function(data, long, lat) {
  
  n_nrow_data <- format(x = nrow(data), big.mark = ",")
  
  world_borders <-
    borders(
      database = "world",
      fill = "white",
      colour = "grey90",
    )
  
  our_map <-
    data %>%
    ggplot() +
    world_borders +
    theme_bw() +
    labs(
      x = "Longitude (decimals)",
      y = "Latitude (decimals)",
      title = paste("Based on ", n_nrow_data, "points")
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      aes(
        x = {{ long }},
        y = {{ lat }}
      ),
      alpha = 0.5,
      size = 0.1
    )
  
  print(our_map)
  
}