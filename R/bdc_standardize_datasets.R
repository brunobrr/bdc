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
#' @param overwrite A logical vector indicating whether the final merged dataset
#' should be overwritten. The default is FALSE.
#' @param format a character setting the output file type. Option available are
#' "csv" and "qs" (recommenced to save large datasets). Default == "csv".
#' @param save_database logical. Should the standardized database be locally
#' saved? Default = FALSE.
#'
#' @return A merged data.frame with column names following Darwin Core
#' terminology.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr pull filter select select_if mutate n everything mutate_if
#' all_of across
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom qs qsave qread
#' @importFrom readr read_csv cols
#'
#' @details
#' `bdc_standardize_datasets()` facilitate the standardization of datasets with
#' different column names by converting them into a new dataset following the
#' Darwin Core terminology. The standardization process relies on a metadata
#' file containing the name, path, and columns that need to be renamed. The
#' metadata file can be constructed using built-in functions (e.g.,
#' `data.frame()`) or storing the information in a CSV file and importing it
#' into R. Regardless of the method chosen, the data frame with metadata needs
#' to contain the following column names (this is a list of required column
#' names; for a comprehensive list of column names following Darwin Core
#' terminology
#' - `datasetName`: A short name identifying the dataset (e.g., GBIF)
#'
#' - `fileName`: The relative path containing the name of the input dataset
#' (e.g., Input_files/GBIF.csv)
#'
#' - `scientificName`: Name of the column in the original database presenting
#' the taxon scientific names with or without authorship information, depending
#' on the format of the source dataset (e.g., Myrcia acuminata)
#'
#' - `decimalLatitude`: Name of the column in the original database presenting
#' the geographic latitude in decimal degrees (e.g., -6.370833)
#'
#' - `decimalLongitude`: Name of the column in the original database presenting
#' the geographic longitude in decimal degrees (e.g., -3.25500)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- readr::read_csv(system.file("extdata/Config/DatabaseInfo.csv",
#'             package = "bdc"))
#'
#' db_standardized <-
#' bdc_standardize_datasets(
#'   metadata = metadata,
#'   format = "csv",
#'   overwrite = TRUE,
#'   save_database = FALSE)
#' }
bdc_standardize_datasets <-
  function(metadata,
           format = "csv",
           overwrite = FALSE,
           save_database = FALSE) {
    fileName <- datasetName <- . <- database_id <- NULL

    switch(EXPR = format,
           qs = {
             merged_filename <-
               here::here("Output", "Intermediate", "00_merged_database.qs")
           },
           csv = {
             merged_filename <-
               here::here("Output", "Intermediate", "00_merged_database.csv")
           })

    # fs::dir_create(here::here("Output", "Intermediate"))

    if (file.exists(merged_filename) & overwrite) {
      unlink(merged_filename)
    }

    if (!file.exists(merged_filename)) {
      # save_in_dir <- here::here("data", "temp_datasets", "/")
      save_in_dir <- file.path(tempdir(), "data/temp_datasets", "/")

      if (!fs::dir_exists(save_in_dir)) {
        fs::dir_create(save_in_dir)
      }

      input_file <-
        metadata %>%
        dplyr::pull(fileName)

      if (length(unique(metadata$datasetName)) != nrow(metadata)) {
        stop("[ERROR]: Dataset names defined in the `datasetName` column must be unique.")
      }

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

        save_in_filename <-
          paste0(save_in_dir, "standard_", dataset_name, ".", format)

        if (!file.exists(save_in_filename) | overwrite == TRUE) {
          base_names <-
            metadata %>%
            dplyr::filter(fileName == input_file[file_index]) %>%
            dplyr::select_if( ~ !is.na(.))

          standard_names <-
            base_names %>%
            names(.)

          required <- c(
            "datasetName",
            "fileName",
            "scientificName",
            "decimalLatitude",
            "decimalLongitude"
          )

          if (!(required %in% standard_names %>% all())) {
            stop(
              paste(
                "Required field is missing. Please check the columns of the",
                dataset_name,
                "in our",
                metadata
              )
            )
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
            readr::read_csv(
              guess_max = 10 ^ 6,
              col_types = readr::cols(.default = "c"),
              n_max = 1,
              show_col_types = F
            )

          skip_to_next <- FALSE

          error_message <-
            paste(
              "[ERROR]: Column names defined in the metadata do not match column names in the",
              dataset_name,
              "file"
            )

          tryCatch(
            if (sum(!vector_for_recode %in% names(imported_raw_dataset)) != 0) {
              cols_not_in_input <- setdiff(vector_for_recode, names(imported_raw_dataset))
              if (length(cols_not_in_input) != 0) {
                message(
                  "[INFO]: Column(s) ",
                  knitr::combine_words(paste0("`", cols_not_in_input, "`"), sep = ", ", and = " and ", oxford_comma = FALSE),
                  " defined in the metadata not present in the ",
                  dataset_name
                )
              }
              ## cols_not_in_metadata <- setdiff(names(imported_raw_dataset), vector_for_recode)
              ## if (length(cols_not_in_metadata) != 0) {
              ##   message("[INFO]: Column(s) ", paste0("`", cols_not_in_metadata, "`", collapse = ", "), " not present in the metadata file.")
              ## }
              stop(error_message)
            } else {
              standard_dataset <-
                input_file[file_index] %>%
                readr::read_csv(trim_ws = F, show_col_types = F) %>%
                dplyr::select(dplyr::all_of(vector_for_recode)) %>%
                purrr::set_names(names(vector_for_recode)) %>%
                dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
                dplyr::select(database_id, dplyr::everything())

              message(paste("Standardizing", base_names$datasetName, "file"))

              standard_dataset <-
                standard_dataset %>%
                dplyr::mutate_if(is.numeric, as.character)

              if (format == "qs") {
                qs::qsave(
                  standard_dataset,
                  paste0(
                    save_in_dir,
                    "standard_",
                    dataset_name,
                    ".",
                    format
                  )
                )
              } else {
                readr::write_csv(
                  standard_dataset,
                  paste0(
                    save_in_dir,
                    "standard_",
                    dataset_name,
                    ".",
                    format
                  )
                )
              }
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

      if (format == "qs") {
        merged_database <-
          # here::here("data", "temp_datasets") %>%
          save_in_dir %>%
          fs::dir_ls(regexp = "*.qs") %>%
          purrr::map_dfr(~ qs::qread(.x) %>%
                         dplyr::mutate(dplyr::across(
                           .cols = dplyr::everything(), ~ as.character(.x)
                         )))
      } else {
        merged_database <-
          # here::here("data", "temp_datasets") %>%
          save_in_dir %>%
          fs::dir_ls(regexp = "*.csv") %>%
          purrr::map_dfr( ~ readr::read_csv(.x, col_types = readr::cols(.default = "c")))
      }

      merged_database <-
        merged_database %>%
        select_if((function(x)
          any(!is.na(x))))


      merged_database <-
        merged_database %>%
        select_if((function(x)
          any(!is.na(x))))

      # should the database be saved?
      if (format == "qs" & save_database == TRUE & overwrite == TRUE) {
        bdc_create_dir()
        qs::qsave(merged_database, merged_filename)
        message(paste("Standardized database was saved in",merged_filename))

      }

      if (format == "csv" & save_database == TRUE & overwrite == TRUE) {
        bdc_create_dir()
        readr::write_csv(merged_database, merged_filename)
        message(paste(merged_filename, "was created"))

      }

      return(merged_database)

    } else {
      message(paste(merged_filename, "already exists!"))

      # Load an existing database
      if (overwrite == FALSE & format == "csv"){
        merged_database <- readr::read_csv(merged_filename,
                                           show_col_types = FALSE)
      }

      if (overwrite == FALSE & format == "qs"){
        merged_database <- qs::qread(merged_filename)
      }

    }

  }
