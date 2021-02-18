
#' Title Create figures reporting the results of the bdc workflow
#'
#' @param data data.frame Containing the results of each test
#' @param tests character string. Containing the names of each test (columns in data starting with ".")
#' @param workflow_step character string. Name to be add as a prefix to names of figures ("prefilter", "taxonomy", "space", "time")
#'
#' @return
#' @export Figures in png format
#'
#' @examples
bdc_create_figures <- function(data, workflow_step = "prefilter") {
  if (workflow_step == "prefilter") {
    tests <-
      c(
        ".missing_name",
        ".missing_xy",
        ".invalid_xy",
        ".xy_provenance",
        ".xy_out_country",
        "bdc_transposed_xy",
        ".summary"
      )
  }


  # function to create barplots
  create_barplot <-
    function(data, column_to_map, workflow_step = workflow_step) {
      temp <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::group_by(database_id, .data[[column_to_map]]) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::mutate(freq = n / sum(n)) %>%
        dplyr::filter(., .data[[column_to_map]] == TRUE)
        
      b <-
        ggplot(temp, aes(x = reorder(database_id, -freq), y = freq)) +
        geom_col(colour = "white", fill = "#1380A1") +
        coord_flip() +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 16),
          legend.position = "top",
          legend.text = element_text(size = 11),
          panel.grid.major.x = element_line(color = "#cbcbcb"),
          panel.grid.major.y = element_blank()
        ) +
        labs(x = "Database", y = "% of valid records ") +
        scale_y_continuous(labels = scales::percent) +
        geom_hline(
          yintercept = 0,
          size = 1,
          colour = "#333333"
        ) +
        geom_label(
          aes(
            x = reorder(database_id, -freq),
            y = freq,
            label = round(freq, 2) * 100
          ),
          hjust = 1,
          vjust = 0.5,
          colour = "white",
          fill = NA,
          label.size = NA,
          # family="Times",
          size = 4
        )

      ggsave(
        paste("output/", "Figures/", workflow_step, "_", column_to_map, "_", 
              "bar",".png", sep = ""),
        b,
        dpi = 300, width = 6, height = 3, units = "cm", scale = 4
      )
    }

  # Names of columns available for creating barplot
  bar <- c(
    ".missing_name", ".missing_xy", ".invalid_xy", ".xy_provenance",
    ".xy_out_country", ".summary"
  )

  # Names of columns available for creating maps
  maps <- c(".xy_out_country")
  
  # Find which names were provided
  w_bar <- intersect(tests, bar)
  w_maps <- intersect(tests, maps)
  w_tranposed <- intersect(tests, "bdc_transposed_xy")
    
  # Create bar plots
  if (length(w_bar) == 0) {
    stop("At least one column name must be provided")
  } else {
    for (i in 1:length(w_bar)) {
      create_barplot(data = data, column_to_map = w_bar[i], 
                    workflow_step = workflow_step)
    }
  }
  
  # Create maps of invalid vs valid records
  if (length(w_maps) == 0) {
    stop("At least one column name must be provided")
  } else {
    for (i in 1:length(w_maps)) {
      d <-
        CoordinateCleaner::cc_val(
          data,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          verbose = F,
          value = "clean"
        )
      
      p <- 
        bdc_quickmap(
          data = d,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          col_to_map = w_maps[i], size = 0.8)
      
      ggsave(
        paste("output/", "Figures/", workflow_step, "_", w_maps[i], "_",
              "map", ".png", sep = ""),
        p,
        dpi = 300, width = 6, height = 3, units = "cm", scale = 4)
    }
  }

  # Create maps of transposed and corrected coordinates
  if (length(w_tranposed) == 0) {
    stop("file 'Output/Check/01_transposed_xy.csv' not found")
  } else {
    temp <- data.table::fread("Output/Check/01_transposed_xy.csv")

    p1 <-
      bdc_quickmap(
        data = temp,
        lon = "decimalLongitude",
        lat = "decimalLatitude",
        col_to_map = "red", size = 1
      )

    p2 <-
      bdc_quickmap(
        data = temp,
        lon = "decimalLongitude_modified",
        lat = "decimalLatitude_modified",
        col_to_map = "blue", size = 1
      )

    p <- cowplot::plot_grid(p1, p2, labels = "AUTO")

    ggsave(paste("output/", "Figures/", workflow_step,  "_",
                 "transposed_xy", "_", "map", ".png", sep = ""),
    p,
    dpi = 300, width = 6, height = 3, units = "cm", scale = 4)
  }
    

  
  message("Check figures in ", here::here("Output", "Figures"))
  
}

