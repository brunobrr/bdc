#' Creates figures reporting the results of the bdc workflow
#'
#' Creates figures (i.e., barplots and maps) reporting the results of data
#' quality tests implemented in the bdc workflow.
#'
#' @param data data.frame. Containing the results of data quality tests and a .
#' @param workflow_step character string. Name of the workflow step. Options available are "prefilter", "space", and "time".
#' @param id character string. The column name with a unique record identifier.
#' Default = "database_id".
#' 
#' @details This function creates figures based on the results of data quality
#' tests implemented. A pre-defined list of test names is used for creating
#' figures depending on the name of the workflow step informed. Figures are
#' saving in "Output/Figures".
#'
#' @return Figures in a png format.
#'
#' @importFrom CoordinateCleaner cc_val
#' @importFrom cowplot plot_grid
#' @importFrom data.table fwrite fread
#' @importFrom dplyr summarise mutate group_by pull intersect filter full_join
#' select summarise_all rename n mutate_if
#' @importFrom fs file_exists
#' @importFrom ggplot2 theme_minimal theme element_text element_line
#' element_blank unit ggplot aes geom_col coord_flip labs geom_hline
#' geom_label scale_y_continuous ggsave geom_histogram
#' @importFrom here here
#' @importFrom readr read_csv
#' @importFrom scales comma
#' @importFrom stats reorder
#' @importFrom tibble as_tibble

#' @export
#'
#' @examples
#' \dontrun{
#' database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
#' lat <- c(-19.93580, -13.01667, -22.34161,  -6.75000, -15.15806)
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
#' bdc_create_figures(data = x, database_id = "database_id",
#' workflow_step = "prefilter")
#' }
bdc_create_figures <-
  function(data,
           database_id = "database_id",
           workflow_step = NULL) {

    # Total number of records
    suppressMessages({
      if (!fs::file_exists("data/n_records.csv")) {
        n_records <-
          data %>%
          dplyr::summarise(n = dplyr::n())

        data.table::fwrite(n_records, "data/n_records.csv")

        # Total number of records per database
        n_record_database <-
          data %>%
          dplyr::mutate(
            database_id = gsub("[[:digit:]]+", "", database_id),
            database_id = gsub("_", "", database_id)
          ) %>%
          dplyr::group_by(database_id) %>%
          dplyr::summarise(n_total = dplyr::n())

        data.table::fwrite(n_record_database, "data/n_record_database.csv")
      } else {
        n_records <-
          readr::read_csv("data/n_records.csv") %>%
          dplyr::pull(n)

        n_record_database <- readr::read_csv("data/n_record_database.csv")
      }
    })

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
          ".summary",
          "summary_all_tests"
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
          "summary_all_tests"
        )

      names_tab <- names(data)
      col_to_tests <- dplyr::intersect(tests, names_tab)
    }

    # time
    if (workflow_step == "time") {
      tests <-
        c(
          "year",
          ".year_outOfRange",
          ".summary",
          "summary_all_tests"
        )
      
      names_tab <- names(data)
      col_to_tests <- dplyr::intersect(tests, names_tab)
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
          # scale_y_continuous(labels = scales::percent) +
          ggplot2::geom_hline(
            yintercept = 0,
            size = 1,
            colour = "#333333"
          ) +
          # ggplot2::geom_label(
          #   ggplot2::aes(
          #     x = stats::reorder(database_id, -freq),
          #     y = freq,
          #     label = n_flagged
          #   ),
          #   hjust = 1,
          #   vjust = 0.5,
          #   colour = "white",
          #   fill = NA,
          #   label.size = NA,
          #   # family="Times",
          #   size = 4,
          #   fontface = "bold"
          # ) +
          ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma)

        ggplot2::ggsave(paste("Output/", "Figures/", workflow_step, "_",
          column_to_map, "_", "BAR", ".png",
          sep = ""
        ),
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
          dplyr::select(starts_with(".")) %>%
          dplyr::mutate_if(is.character, ~as.logical(as.character(.))) %>% 
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

        b <-
          ggplot2::ggplot(temp, ggplot2::aes(x = stats::reorder(Name, -freq), y = freq)) +
          ggplot2::geom_col(colour = "white", fill = "royalblue") +
          ggplot2::coord_flip() +
          our_theme +
          ggplot2::labs(x = "Tests", y = "% of records flagged") +
          # scale_y_continuous(labels = scales::percent) +
          ggplot2::geom_hline(
            yintercept = 0,
            size = 1,
            colour = "#333333"
          ) +
          # ggplot2::geom_label(
          #   ggplot2::aes(
          #     x = stats::reorder(Name, -freq),
          #     y = freq,
          #     label = n_flagged
          #   ),
          #   hjust = 1,
          #   vjust = 0.5,
          #   colour = "white",
          #   fill = NA,
          #   label.size = NA,
          #   size = 3,
          #   fontface = "bold"
          # ) +
          ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::comma)

        ggplot2::ggsave(paste("Output/", "Figures/", workflow_step, "_",
                              column_to_map,
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
      ".scientificName_empty",
      ".coordinates_empty",
      ".coordinates_outOfRange",
      ".invalid_basis_of_records",
      ".coordinates_country_inconsistent",
      ".summary",
      ".uncer_term",
      "names_not_found",
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

    # Names of columns available for creating maps
    maps <- c(".coordinates_country_inconsistent", ".cap", ".cen", ".inst")

    # Names of column available for creating histogram
    hist <- c("year")

    # Find which names were provided
    w_bar <- dplyr::intersect(col_to_tests, bar)
    w_maps <- dplyr::intersect(col_to_tests, maps)
    w_tranposed <- dplyr::intersect(col_to_tests, "coordinates_transposed")
    w_hist <- dplyr::intersect(col_to_tests, hist)

    # Create bar plots
    if (length(w_bar) == 0 & workflow_step %in% c("prefilter", "space")) {
      message("At least one column 'starting with '.' must be provided")
    }

    if (length(w_bar) != 0) {
      w <- which(colSums(!data[{{ w_bar }}]) == 0)

      if (length(w) != 0) {
        message(
          "No records flagged for the following tests:\n",
          paste(w_bar[w], collapse = " ")
        )
        w_bar <- w_bar[-w]
      }

      for (i in 1:length(w_bar)) {
        create_barplot_database(
          data = data,
          column_to_map = w_bar[i],
          workflow_step = workflow_step
        )
      }

      create_barplot_all_tests(
        data = data,
        column_to_map = "summary_all_tests",
        workflow_step = workflow_step
      )
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
          dplyr::select({{ w_maps }}[i], decimalLongitude, decimalLatitude) %>%
          dplyr::filter(. == FALSE)

        p <-
          bdc_quickmap(
            data = d,
            lon = "decimalLongitude",
            lat = "decimalLatitude",
            col_to_map = w_maps[i], size = 0.8
          )

        ggplot2::ggsave(
          paste("Output/", "Figures/", workflow_step, "_", w_maps[i], "_",
            "MAP", ".png",
            sep = ""
          ),
          p,
          dpi = 300, width = 6, height = 3, units = "cm", scale = 4
        )
      }
    }

    # Create maps of transposed and corrected coordinates
    if (length(w_tranposed) == 0 & workflow_step == "prefilter") {
      message("file 'Output/Check/01_coordinates_transposed.csv' not found")
    }

    if (length(w_tranposed) != 0) {
      temp <- data.table::fread("Output/Check/01_coordinates_transposed.csv")

      p1 <-
        bdc_quickmap(
          data = temp,
          lon = "decimalLongitude",
          lat = "decimalLatitude",
          col_to_map = "#EC364F",
          size = 1
        )

      p2 <-
        bdc_quickmap(
          data = temp,
          lon = "decimalLongitude_modified",
          lat = "decimalLatitude_modified",
          col_to_map = "royalblue",
          size = 1
        )
      p <- cowplot::plot_grid(p1, p2, labels = "AUTO", scale = 1)

      ggplot2::ggsave(paste("Output/", "Figures/", workflow_step, "_",
        "coordinates_transposed", "_", "MAP", ".png",
        sep = ""
      ),
      p,
      dpi = 300, width = 6, height = 3, units = "cm", scale = 4
      )
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
        ggplot2::ggplot(ggplot2::aes(x = year)) +
        ggplot2::geom_histogram(
          colour = "white",
          fill = "royalblue", position = "identity"
        ) +
        our_theme +
        ggplot2::labs(x = "Year", y = "Number of records") +
        ggplot2::geom_hline(
          yintercept = 0,
          size = 1,
          colour = "#333333"
        )

      ggplot2::ggsave(
        paste("Output/", "Figures/", workflow_step, "_", "year", "_",
          "BAR", ".png",
          sep = ""
        ),
        p,
        dpi = 300, width = 6, height = 3, units = "cm", scale = 5
      )
    }

    message("Check figures in ", here::here("Output", "Figures"))
  }
