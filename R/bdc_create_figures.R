
#' Create figures reporting the results of the bdc workflow
#' @description 
#' @param data data.frame Containing the results of each test
#' @param tests character string. Containing the names of each test (columns in data starting with ".")
#' @param workflow_step character string. Name to be add as a prefix to names of figures ("prefilter", "taxonomy", "space", "time")
#'
#' @return
#' @export Figures in png format
#'
#' @examples
bdc_create_figures <- function(data, workflow_step = NULL) {
  
  # Total number of records
  suppressMessages({
  if (!file_exists("data/n_records.csv")) {
    n_records <-
      data %>%
      dplyr::summarise(n = n())
    
    data.table::fwrite(n_records, "data/n_records.csv")
    
    # Total number of records per database
    n_record_database <-
      data %>%
      dplyr::mutate(
        database_id = gsub("[[:digit:]]+", "", database_id),
        database_id = gsub("_", "", database_id)
      ) %>%
      dplyr::group_by(database_id) %>%
      dplyr::summarise(n_total = n())
    
    data.table::fwrite(n_record_database, "data/n_record_database.csv")
  } else {
    n_records <- read_csv("data/n_records.csv") %>% dplyr::pull(n)
    n_record_database <- read_csv("data/n_record_database.csv")
  }
  })
  
  our_theme <- 
    theme_minimal() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 12),
      panel.grid.major.x = element_line(color = "#cbcbcb"),
      panel.grid.major.y = element_blank(), 
      plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")
    ) 
  
  # prefilter
  if (workflow_step == "prefilter") {
    tests <-
      c(
        ".missing_name",
        ".missing_xy",
        ".invalid_xy",
        ".xy_provenance",
        ".xy_out_country",
        ".summary", 
        "summary_all_tests"
      )
    
    names_tab <- names(data)
    col_to_tests <- intersect(tests, names_tab)
    
    if (file.exists("Output/Check/01_transposed_xy.csv")) {
      col_to_tests <- c(col_to_tests, "transposed_xy")
    }
  }
  
  # space
  if (workflow_step == "space") {
    tests <-
      c(
        ".equ",
        ".zer",
        ".cap",
        ".cen",
        ".otl",
        ".gbf",
        ".inst",
        ".dpl",
        ".rou",
        "summary_all_tests"
      )
    
    names_tab <- names(data)
    col_to_tests <- intersect(tests, names_tab)
  }
  
  # time
  if (workflow_step == "time"){ 
    names_tab <- names(data)
    col_to_tests <- intersect("year", names_tab)
  }
  
  # function to create barplots for each dataset separately
  create_barplot_database <-
    function(data, column_to_map, workflow_step = workflow_step) {
      temp <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::filter(., .data[[column_to_map]] == FALSE) %>%
        dplyr::group_by(database_id, .data[[column_to_map]]) %>%
        dplyr::summarise(n_flagged = n()) %>%
        dplyr::full_join(., n_record_database, by = "database_id") %>%
        dplyr::mutate(freq = round(n_flagged  / n_total, 5))
      
      temp[is.na(temp)] <- 0
      
      b <-
        ggplot(temp, aes(x = reorder(database_id,-freq), y = freq)) +
        geom_col(colour = "white", fill = "royalblue") +
        coord_flip() +
        our_theme +
        labs(x = "Dataset", y = "% of records flagged") +
        # scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0,
                   size = 1,
                   colour = "#333333") +
        geom_label(
          aes(
            x = reorder(database_id,-freq),
            y = freq,
            label = n_flagged
          ),
          hjust = 1,
          vjust = 0.5,
          colour = "white",
          fill = NA,
          label.size = NA,
          # family="Times",
          size = 4,
          fontface = "bold"
        ) +
        scale_y_continuous(expand = c(0, 0), labels = scales::comma)
      
      ggsave(paste("output/", "Figures/", workflow_step, "_", column_to_map,
          "_", "BAR", ".png", sep = ""),
        b,
        dpi = 300,
        width = 6,
        height = 3,
        units = "cm",
        scale = 5
      )
    }

  # function to create barplots considering all datasets together
  create_barplot_all_tests <-
    function(data,
             column_to_map = "summary_all_tests",
             workflow_step = workflow_step) {
      temp <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(freq =
                        round((V1 / n_records * 100), 2)) %>%
        dplyr::rename(Name = `NA`,
                      n_flagged = V1)
      
      temp[is.na(temp)] <- 0
      
      b <-
        ggplot(temp, aes(x = reorder(Name,-freq), y = freq)) +
        geom_col(colour = "white", fill = "royalblue") +
        coord_flip() +
        our_theme +
        labs(x = "Tests", y = "% of records flagged") +
        # scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0,
                   size = 1,
                   colour = "#333333") +
        geom_label(
          aes(
            x = reorder(Name,-freq),
            y = freq,
            label = n_flagged
          ),
          hjust = 1,
          vjust = 0.5,
          colour = "white",
          fill = NA,
          label.size = NA,
          size = 3,
          fontface = "bold"
        ) +
        scale_y_continuous(expand = c(0, 0), labels = scales::comma)
      
      ggsave(paste("output/", "Figures/", workflow_step, "_", column_to_map,
          "_", "BAR", ".png", sep = ""),
        b,
        dpi = 300,
        width = 6,
        height = 3,
        units = "cm",
        scale = 5
      )
    }
  
  # Names of columns available for creating barplot
  bar <- c(
    ".missing_name", ".missing_xy", ".invalid_xy", ".xy_provenance",
    ".xy_out_country", ".summary", ".uncer_term", "names_not_found", 
    ".equ", ".zer", ".cap",".cen", ".otl", ".gbf", ".inst", ".dpl",
    ".rou", "summary_all_tests"
  )

  # Names of columns available for creating maps
  maps <- c(".xy_out_country", ".cap", ".cen", ".inst")
  
  # Names of column available for creating histogram
  hist <- "year"
  
  # Find which names were provided
  w_bar <- intersect(col_to_tests, bar)
  w_maps <- intersect(col_to_tests, maps)
  w_tranposed <- intersect(col_to_tests, "transposed_xy")
  w_hist <- intersect(col_to_tests, hist)
  
  # Create bar plots
  if (length(w_bar) == 0 & workflow_step %in% c('prefilter', 'space')) {
    message("At least one column 'starting with '.' must be provided")
  }  
  if (length(w_bar) != 0){
     w <- which(colSums(!data[{{ w_bar}} ]) == 0)
     
     if (length(w) != 0){
     message("No records flagged for the following tests:\n", 
             paste(w_bar[w], collapse = " "))
     w_bar <- w_bar[-w]
     }
     
    for (i in 1:length(w_bar)) {
      create_barplot_database(
        data = data,
        column_to_map = w_bar[i],
        workflow_step = workflow_step
      )
    }
    
    create_barplot_all_tests(data = data,
                             column_to_map = "summary_all_tests",
                             workflow_step = workflow_step)
    
  }
  
  # Create maps of invalid vs valid records
  if (length(w_maps) == 0 & workflow_step %in% c('prefilter', 'space')) {
    message("At least one column 'starting with '.' must be provided")
  } 
  
  if (length(w_maps) != 0) {
    w <- which(colSums(!data[{{ w_maps}} ]) == 0)
    
    if (length(w) != 0){
    message("No records flagged for the following tests:\n", 
            paste(w_maps[w], collapse = " "))
    w_maps <- w_maps[-w]
    }
    
    for (i in 1:length(w_maps)) {
      d <-
        CoordinateCleaner::cc_val(
          data,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          verbose = F,
          value = "clean"
        )
      
      d <-
        d %>%
        dplyr::select({{w_maps}}[i], decimalLongitude, decimalLatitude) %>%
        dplyr::filter(. == FALSE)
      
      p <- 
        bdc_quickmap(
          data = d,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          col_to_map = w_maps[i], size = 0.8)
      
      ggsave(
        paste("output/", "Figures/", workflow_step, "_", w_maps[i], "_",
              "MAP", ".png", sep = ""),
        p,
        dpi = 300, width = 6, height = 3, units = "cm", scale = 4)
    }
  }

  # Create maps of transposed and corrected coordinates
  if (length(w_tranposed) == 0 & workflow_step == "prefilter") {
    message("file 'Output/Check/01_transposed_xy.csv' not found")
  } 
  
  if (length(w_tranposed) != 0) {
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
                 "transposed_xy", "_", "MAP", ".png", sep = ""),
    p,
    dpi = 300, width = 6, height = 3, units = "cm", scale = 4)
  }
    
  if (length(w_hist) == 0 & workflow_step == "time") {
    message("Column 'year' not found")
  } 
  
  if (length(w_hist) != 0) {
  data <- 
    data %>% 
    dplyr::select(year) %>% 
    dplyr::filter(!is.na(year))
  
  p <- data %>%
    ggplot(aes(x=year)) +
    geom_histogram(colour = "white", fill = "royalblue", position = 'identity') +
    our_theme +
    labs(x = "Year", y = "Number of records") +
    geom_hline(
      yintercept = 0,
      size = 1,
      colour = "#333333"
    )
  
  ggsave(
    paste("output/", "Figures/", workflow_step, "_", "year", "_", 
          "BAR",".png", sep = ""),
    p,
    dpi = 300, width = 6, height = 3, units = "cm", scale = 5
  )
  }
  
  message("Check figures in ", here::here("Output", "Figures"))
  
}