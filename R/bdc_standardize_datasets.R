#' Standardize datasets columns based on metadata
#'
#' This function's main goal is to merge and standardize different
#' datasets into a new dataset with column names following the Darwin
#' Core terminology. All the process is based on a metadata file
#' provided by the user.
#'
#' @param metadata A data frame with metadata containing information about the
#' name, path, and columns of the original data set which need to be
#' renamed. See @details.
#'
#' @param overwrite A logical vector indicating whether the final merged
#' dataset should be overwritten. The default is FALSE.
#'
#' @importFrom data.table fread
#' @importFrom dplyr pull filter select select_if mutate n everything mutate_if
#' all_of
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom qs qsave
#' @importFrom vroom vroom cols
#'
#' @details
#' `bdc_standardize_datasets()` facilitate the standardization of datasets with
#' different column names converting them into a new dataset following the
#' Darwin Core terminology. The standardization process relies on a metadata
#' file containing the name, path, and columns that need to be renamed. The
#' metadata file can be constructed using built-in functions (e.g.,
#' `data.frame()`) or storing the information in a CSV file and importing it
#' into R. Regardless of the method chosen, the data frame with metadata needs
#' to contain the following column names (this is a list of required column
#' names; for a comprehensive list of column names following Darwin Core
#' terminology, see [here](https://dwc.tdwg.org/terms/:
#'
#' - `datasetName`: A short name identifying the dataset (e.g., GBIF)
#'
#' - `fileName`: The relative path containing the name of the input dataset (e.g.,
#' Input_files/GBIF.csv)
#'
#' - `scientificName`: Name of the column in the original database presenting
#' the scientific names of the taxon with or without authorship information,
#' depending on the format of the source dataset (e.g., Myrcia acuminata)
#'
#' - `decimalLatitude`: Name of the column in the original database presenting
#' the geographic latitude in decimal degrees (e.g., -6.370833)
#'
#'
#' - `decimalLongitude`: Name of the column in the original database presenting
#' the geographic longitude in decimal degrees (e.g., -3.25500)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  metadata <- system.file("extdata", "Config/DatabaseInfo.csv", package = "bdc")
#   # TODO: finish example
#' }
bdc_standardize_datasets <- function(metadata, overwrite = FALSE) {

  fs::dir_create(here::here("Output", "Intermediate"))

  merged_filename <- here::here("Output", "Intermediate", "00_merged_database.qs")

  if (file.exists(merged_filename) & overwrite) {

    unlink(merged_filename)

  }

  if (!file.exists(merged_filename)) {

    save_in_dir <- here::here("data", "temp_datasets")

    if (!fs::dir_exists(save_in_dir)) {

      fs::dir_create(save_in_dir)

    }

    input_file <-
      metadata %>%
      dplyr::pull(fileName)

    for (file_index in seq_along(input_file)) {

      input_filename <-
        metadata %>%
        dplyr::filter(fileName == input_file[file_index]) %>%
        dplyr::pull(fileName)

      dataset_name <-
        metadata %>%
        dplyr::filter(fileName == input_file[file_index]) %>%
        dplyr::select(datasetName) %>%
        dplyr::pull()

      save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".qs")

      if (!file.exists(save_in_filename)) {

        base_names <-
          metadata %>%
          dplyr::filter(fileName == input_file[file_index]) %>%
          dplyr::select_if(~ !is.na(.))

        standard_names <-
          base_names %>%
          names(.)

        required <- c(
          "datasetName", "fileName", "scientificName",
          "decimalLatitude", "decimalLongitude"
        )

        if (!(required %in% standard_names %>% all())) {

          stop(paste("Required field is missing. Please check the columns of the", dataset_name, "in our", metadata))

        }

        basename_names <- base_names %>%
          dplyr::select(-datasetName, -fileName)

        standard_names <-
          basename_names %>%
          names(.)

        vector_for_recode <-
          basename_names %>%
          purrr::set_names(standard_names) %>%
          {
            c(.)
          } %>%
            unlist()

          imported_raw_dataset <-
           input_file[file_index] %>%
            vroom::vroom(guess_max = 10^6, col_types = vroom::cols(.default = "c"), n_max = 1)

          skip_to_next <- FALSE

          error_message <-
            paste("[ERROR]: Column names defined in the metadata do not match column names in the", dataset_name, "file")

          tryCatch(

            if (sum(!vector_for_recode %in% names(imported_raw_dataset)) != 0) {

              stop(error_message)

            } else {

              standard_dataset <-
                input_file[file_index] %>%
                data.table::fread() %>%
                dplyr::select(dplyr::all_of(vector_for_recode)) %>%
                purrr::set_names(names(vector_for_recode)) %>%
                dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
                dplyr::select(database_id, dplyr::everything())

              message(paste("Creating", save_in_filename))

              standard_dataset %>%
                dplyr::mutate_if(is.numeric, as.character) %>%
                qs::qsave(save_in_filename)

            },

            error = function(e) {
              message(error_message)
              skip_to_next <<- TRUE
            }

          )

          if (skip_to_next) {

            next

          }

      } else {

        message(paste(save_in_filename, "already exists!"))

      }

    }

    # Concatenate all the resulting standardized databases
    merged_database <-
      here::here("data", "temp_datasets") %>%
      fs::dir_ls(regexp = "*.qs") %>%
      plyr::ldply(.data = .,
        .fun = qs::qread,
        .progress = plyr::progress_text(char = "."),
        .id = NULL)

      ## merged_database %>%
      ##   mutate(database_name = str_remove(database_id, "_[0-9].*")) %>%
      ##   distinct(database_name)

      ## waldo::compare(
      ##   x = merged_database %>% names(),
      ##   y = metadata %>% names()
      ## )

      merged_database <-
        merged_database %>%
        select_if((function(x) any(!is.na(x))))

      merged_database %>%
        qs::qsave(merged_filename)

      message(paste("Merged database saved in", merged_filename))

  } else {

    message(paste(merged_filename, "already exists!"))

  }

}
