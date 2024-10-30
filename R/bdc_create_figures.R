#' Create figures reporting the results of the bdc package
#'
#' Creates figures (i.e., bar plots, maps, and histograms) reporting the results
#' of data quality tests implemented in the bdc package.
#'
#' @param data data.frame. Containing the results of data quality tests; that
#' is, columns starting wit ".".
#' @param workflow_step character string. Name of the workflow step. Options
#' available are "prefilter", "space", and "time".
#' @param bins_maps character. Number of bins used to create the map.
#' @param database_id character string. The column name with a unique record
#' identifier. Default = "database_id".
#' @param save_figures logical. Should the figures be saved for further
#' inspection? Default = FALSE.
#'
#' @details This function creates figures based on the results of data quality
#' tests implemented. A pre-defined list of test names is used for creating
#' figures depending on the name of the workflow step informed. Figures are
#' saved in "Output/Figures" if save_figures == TRUE.
#'
#' @return List containing figures showing the results of data quality test
#' implemented in one module of bdc. When save_figures = TRUE, figures are
#' also saved locally in a png format.
#'
#' @importFrom CoordinateCleaner cc_val
#' @importFrom readr read_csv
#' @importFrom dplyr summarise n pull mutate group_by intersect filter full_join
#' select mutate_if summarise_all rename
#' @importFrom ggplot2 theme_minimal theme element_text element_line
#' element_blank unit ggplot aes geom_col coord_flip labs geom_hline
#' scale_y_continuous ggsave theme_void geom_polygon geom_hex coord_quickmap
#' scale_fill_viridis_c geom_histogram
#' @importFrom here here
#' @importFrom stats reorder
#' @importFrom tibble as_tibble
#' @importFrom tidyselect starts_with
#' @export
#'
#' @examples
#' \dontrun{
#' database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
#' lat <- c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806)
#' lon <- c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861)
#' .scientificName_emptys <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
#' .coordinates_empty <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
#' .invalid_basis_of_records <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#' .summary <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
#'
#' x <- data.frame(
#'   database_id,
#'   lat,
#'   lon,
#'   .scientificName_emptys,
#'   .coordinates_empty,
#'   .invalid_basis_of_records,
#'   .summary
#' )
#'
#'figures <- 
#' bdc_create_figures(
#'   data = x, 
#'   database_id = "database_id",
#'   workflow_step = "prefilter",
#'   save_figures = FALSE
#' )
#' }
bdc_create_figures <-
  function(data,
           database_id = "database_id",
           workflow_step = NULL,
           bins_maps = 15,
           save_figures = FALSE) {
    . <- .data <- n_flagged <- n_total <- freq <- NULL
    . <- V1 <- Name <- freq <- year <- decimalLongitude <- NULL
    decimalLatitude <- . <- long <- lat <- group <- `NA` <- .summary <- NULL
    
    suppressWarnings({
      check_require_cran("cowplot")
      check_require_cran("readr")
      check_require_cran("rworldmap")
      check_require_cran("ggplot2")
      check_require_cran("hexbin")
      
    })
    
    match.arg(
      arg = workflow_step,
      choices = c("prefilter", "space", "time")
    )
    
    temp <- data %>% dplyr::select(tidyselect::starts_with("."))
    
    if (all((colSums(temp, na.rm = TRUE) - nrow(temp)) == 0)) {
      message("Figures were not created.\nNo records flagged as 'FALSE' in columns starting with '.'")
    }
    
    if (ncol(temp) == 0) {
      message(
        "Figures were not created.\nAt least one column 'starting with '.' containing results of data-quality tests must be provided"
      )
    }
    
    bdc_create_dir()
    
    # Formatting y axis of ggplot bar
    fancy_scientific <- function(l) {
      format(l, big.mark = ",", digits = 2, nsmall = 1)
    }
    
    # Total number of records
    suppressMessages({
      n_records <-
        data %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull(n)
      
      # Total number of records per database
      n_record_database <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::group_by(database_id) %>%
        dplyr::summarise(n_total = dplyr::n())
      
      our_theme <-
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = 18),
          axis.text = ggplot2::element_text(size = 12),
          panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
          panel.grid.major.y = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
        )
      
      # prefilter
      if (workflow_step == "prefilter") {
        tests <-
          c(
            ".scientificName_empty",
            ".coordinates_empty",
            ".coordinates_outOfRange",
            ".invalid_basis_of_records",
            ".coordinates_country_inconsistent",
            ".summary"
          )
        
        names_tab <- names(data)
        col_to_tests <- dplyr::intersect(tests, names_tab)
        
        if (file.exists("Output/Check/01_coordinates_transposed.csv")) {
          col_to_tests <- c(col_to_tests, "coordinates_transposed")
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
            ".urb",
            ".summary"
          )
        
        names_tab <- names(data)
        col_to_tests <- dplyr::intersect(tests, names_tab)
      }
      
      # time
      if (workflow_step == "time") {
        tests <-
          c(
            ".eventDate_empty",
            ".year_outOfRange",
            ".summary",
            "summary_all_tests"
          )
        
        names_tab <- names(data)
        col_to_tests <- dplyr::intersect(tests, names_tab)
      }
      
      # function to create bar plots for each dataset separately
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
            dplyr::summarise(n_flagged = dplyr::n()) %>%
            dplyr::full_join(., n_record_database, by = "database_id") %>%
            dplyr::mutate(freq = round(n_flagged / n_total, 5) * 100)
          
          temp[is.na(temp)] <- 0
          
          b <-
            ggplot2::ggplot(temp, ggplot2::aes(x = stats::reorder(database_id, -freq), y = freq)) +
            ggplot2::geom_col(colour = "white", fill = "royalblue") +
            ggplot2::coord_flip() +
            our_theme +
            ggplot2::labs(x = "Dataset", y = "% of records flagged") +
            ggplot2::geom_hline(
              yintercept = 0,
              size = 1,
              colour = "#333333"
            ) +
            ggplot2::scale_y_continuous(expand = c(0, 0), 
                                        labels = fancy_scientific)
          
        }
      
      # function to create barplots considering all datasets together
      create_barplot_all_tests <-
        function(data,
                 column_to_map = "summary_all_tests",
                 workflow_step = workflow_step) {
          temp <-
            data %>%
            dplyr::select(tidyselect::starts_with(".")) %>%
            dplyr::mutate_if(is.character, ~ as.logical(as.character(.))) %>%
            dplyr::summarise_all(., .funs = sum) %>%
            t() %>%
            tibble::as_tibble(rownames = "NA") %>%
            dplyr::mutate(V1 = nrow(data) - V1) %>%
            dplyr::mutate(
              freq =
                round((V1 / n_records * 100), 2)
            ) %>%
            dplyr::rename(
              Name = `NA`,
              n_flagged = V1
            )
          
          temp[is.na(temp)] <- 0
          
          if (all(temp$freq == 0)) {
            temp$freq <- 0.000001
          } else {
            gg <-
              ggplot2::ggplot(temp, ggplot2::aes(
                x = stats::reorder(Name, -freq),
                y = freq
              ))
          }
          
          b <-
            gg +
            ggplot2::geom_col(colour = "white", fill = "royalblue") +
            ggplot2::coord_flip() +
            our_theme +
            ggplot2::labs(x = "Tests", y = "% of records flagged") +
            ggplot2::geom_hline(
              yintercept = 0,
              size = 1,
              colour = "#333333"
            ) +
            ggplot2::scale_y_continuous(expand = c(0, 0), 
                                        labels = fancy_scientific)
          
        }
      
      # Names of columns available for creating barplot
      bar <- c(
        ".scientificName_empty",
        ".coordinates_empty",
        ".coordinates_outOfRange",
        ".invalid_basis_of_records",
        ".coordinates_country_inconsistent",
        ".uncer_term",
        ".rou",
        ".equ",
        ".zer",
        ".cap",
        ".cen",
        ".otl",
        ".gbf",
        ".inst",
        ".urb",
        ".dpl",
        ".eventDate_empty",
        ".year_outOfRange",
        ".summary",
        "summary_all_tests"
      )
      
      # Names of columns available for creating maps
      maps <-
        c(
          ".coordinates_country_inconsistent",
          ".equ",
          ".cap",
          ".cen",
          ".otl",
          ".inst",
          ".urb",
          ".dpl",
          ".rou"
        )
      
      # Names of column available for creating histogram
      hist <- c("year")
      
      # Find which names were provided
      w_bar <- dplyr::intersect(col_to_tests, bar)
      w_maps <- dplyr::intersect(col_to_tests, maps)
      w_tranposed <- dplyr::intersect(col_to_tests, "coordinates_transposed")
      w_hist <- hist
      
      # List for saving figures
      res <- list()
      
      # Create bar plots
      if (length(w_bar) == 0 & workflow_step %in% c("prefilter", "space")) {
        message("At least one column 'starting with '.' must be provided")
      }
      
      if (length(w_bar) != 0) {
        w <- which(colSums(!data[{{ w_bar }}], na.rm = TRUE) == 0)
        
        if (length(w) != 0) {
          message(
            "No records flagged for the following tests:\n",
            paste(w_bar[w], collapse = " ")
          )
          w_bar <- w_bar[-w]
        }
        
        if (n_records != 1 & length(w_bar) != 0) {
          for (i in 1:length(w_bar)) {
            bp <- 
              create_barplot_database(
                data = data,
                column_to_map = w_bar[i],
                workflow_step = workflow_step
              )
            
            bp_list <- list(bp)
            names(bp_list) <-  w_bar[i]
            res <- c(res, bp_list)
          }
        }
        
        # summary of all tests
        if (n_records != 1 & length(w_bar) != 0) {
          bp_all <-
            create_barplot_all_tests(
              data = data,
              column_to_map = "summary_all_tests",
              workflow_step = workflow_step)
          
          bp_all_list <- list(bp_all)
          names(bp_all_list) <- "summary_all_tests"
          res <- c(res, bp_all_list)
        }
        
      }
      
      # Create maps of invalid vs valid records
      if (length(w_maps) == 0 & workflow_step %in% c("prefilter", "space")) {
        message("At least one of the following columns must be provided for creating maps\n", paste0(maps, sep = " "))
      }
      
      if (length(w_maps) != 0) {
        w <- which(colSums(!data[{{ w_maps }}]) == 0)
        
        if (length(w) != 0) {
          message(
            "No records flagged for the following tests:\n",
            paste(w_maps[w], collapse = " ")
          )
          w_maps <- w_maps[-w]
        }
        
        m <- rnaturalearth::ne_countries(returnclass = "sf")
        
        # new theme
        our_theme2 <-
          ggplot2::theme_void() +
          ggplot2::theme(
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
          )
        
        for (i in 1:length(w_maps)) {
          d <-
            CoordinateCleaner::cc_val(
              data,
              lon = "decimalLongitude",
              lat = "decimalLatitude",
              verbose = FALSE,
              value = "clean"
            )
          
          d <-
            d %>%
            dplyr::select({{ w_maps }}[i], decimalLongitude, decimalLatitude) %>%
            dplyr::filter(.data[[w_maps[i]]] == FALSE)
          
          if (w_maps[i] == ".cen") {
            p <-
              bdc_quickmap(
                data = d,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                col_to_map = "blue",
                size = 0.9
              )
            
            p_list <- list(p)
            names(p_list) <-  w_maps[i]
            res <- c(res, p_list)
          } else {
            if (nrow(d) > 0) {
              p <-
                ggplot2::ggplot() +
                ggplot2::geom_sf(
                  data = m,
                  # ggplot2::aes(x = long, y = lat, group = group),
                  fill = "gray60",
                  colour = "gray88") +
                ggplot2::geom_hex(
                  data = d,
                  ggplot2::aes(x = decimalLongitude, y = decimalLatitude),
                  stat = "binhex",
                  na.rm = TRUE,
                  bins = bins_maps
                ) +
                # ggplot2::coord_quickmap +
                ggplot2::coord_sf() +
                ggplot2::theme_void() +
                ggplot2::labs(fill = "# of Records") +
                ggplot2::scale_fill_viridis_c() +
                our_theme2
              
              p_list <- list(p)
              names(p_list) <-  w_maps[i]
              res <- c(res, p_list)
            } # if
          } # else
        } # for
      } # if (length(w_maps)
      
      # Create maps of transposed and corrected coordinates
      if (length(w_tranposed) == 0 & workflow_step == "prefilter") {
        message("file 'Output/Check/01_coordinates_transposed.csv' not found. Maps showing the results of bdc_coordinates_transposed test will not be created")
      }
      
      if (length(w_tranposed) != 0) {
        temp <- readr::read_csv("Output/Check/01_coordinates_transposed.csv")
        
        p1 <-
          bdc_quickmap(
            data = temp,
            lon = "decimalLongitude",
            lat = "decimalLatitude",
            col_to_map = "#EC364F",
            size = 0.7
          )
        
        p2 <-
          bdc_quickmap(
            data = temp,
            lon = "decimalLongitude_modified",
            lat = "decimalLatitude_modified",
            col_to_map = "royalblue",
            size = 0.7
          )
        
        pt <- cowplot::plot_grid(p1, p2, nrow = 2)
        
        pt_list <- list(pt)
        names(pt_list) <-  "coordinates_transposed"
        res <- c(res, pt_list)
      }
      
      if (length(w_hist) == 0 & workflow_step == "time") {
        message("Column 'year' not found")
      }
      
      if (length(w_hist) != 0 & workflow_step == "time") {
        data <-
          data %>%
          dplyr::filter(.summary == TRUE)
        
        min_year <- min(data$year, na.rm = T)
        max_year <- max(data$year, na.rm = T)
        
        t <- data %>%
          ggplot2::ggplot(ggplot2::aes(x = year)) +
          ggplot2::geom_histogram(
            colour = "white",
            fill = "royalblue", position = "identity", bins = 80
          ) +
          our_theme +
          ggplot2::labs(x = "Year", y = "Number of records") +
          ggplot2::geom_hline(
            yintercept = 0,
            linewidth = 1,
            colour = "#333333"
          ) +
          ggplot2::scale_y_continuous(labels = fancy_scientific)
        
        t_list <- list(t)
        names(t_list) <-  "year"
        res <- c(res, t_list)
        
      }
    }) # suppresswarning
    
    if (save_figures == TRUE){
      
      for (i in seq_along(res)) {
        column_to_map <- names(res)[i]
        ggplot2::ggsave(
          paste(
            here::here("Output", "Figures"),
            "/",
            workflow_step,
            "_",
            column_to_map,
            "_",
            "BAR",
            ".png",
            sep = ""
          ),
          res[[i]],
          dpi = 300,
          width = 6,
          height = 3,
          units = "cm",
          scale = 5
        )
      }
      
      message("Check figures in ", here::here("Output", "Figures"))
    }
    
    return(res)
  }
