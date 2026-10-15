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
#' should be overwritten. The default is FALSE. When FALSE and the merged file
#' (`Output/Intermediate/00_merged_database.<format>`) already exists, that
#' file is read and returned without processing `metadata`.
#' @param format a character setting the output file type. Option available are
#' "csv" and "qs2" (recommenced to save large datasets). Default == "csv".
#' @param save_database logical. Should the standardized database be locally
#' saved? Default = FALSE.
#'
#' @return A merged data.frame with column names following Darwin Core
#' terminology and all columns stored as character. It contains only the
#' datasets described in `metadata`; a dataset whose file lacks a column named
#' in the metadata, or that fails to be read, is excluded and named in a
#' message.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr pull filter select select_if mutate n everything mutate_if
#' all_of across
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom qs2 qs_save qs_read
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
#' terminology, see
#' [here] (https://brunobrr.github.io/bdc/articles/integrate_datasets.html)
#'
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
    database_id <- NULL

    format <- match.arg(format, c("csv", "qs2"))

    merged_filename <-
      here::here(
        "Output", "Intermediate", paste0("00_merged_database.", format)
      )

    if (file.exists(merged_filename) && overwrite) {
      unlink(merged_filename)
    }

    if (file.exists(merged_filename)) {
      message(paste(merged_filename, "already exists!"))

      if (format == "qs2") {
        merged_database <- qs2::qs_read(merged_filename)
      } else {
        merged_database <-
          readr::read_csv(
            merged_filename,
            col_types = readr::cols(.default = "c"),
            show_col_types = FALSE
          )
      }

      return(merged_database)
    }

    if (length(unique(metadata$datasetName)) != nrow(metadata)) {
      stop("[ERROR]: Dataset names defined in the `datasetName` column must be unique.")
    }

    bdc_check_required_metadata(metadata)

    # Each call writes its intermediate files to its own directory and merges
    # only the files it wrote, so datasets from earlier calls never leak in.
    save_in_dir <- tempfile("bdc_standardize_")
    fs::dir_create(save_in_dir)
    on.exit(unlink(save_in_dir, recursive = TRUE), add = TRUE)
    written_files <- character(0)

    # A dataset that fails to be read or written is left out of the merge.
    exclude_dataset <- function(dataset_name, e) {
      message(
        "[ERROR]: Could not standardize the ", dataset_name,
        " file, which is excluded from the merged database: ",
        conditionMessage(e)
      )
      FALSE
    }

    for (row_index in seq_len(nrow(metadata))) {
      dataset_metadata <-
        metadata[row_index, , drop = FALSE] %>%
        dplyr::select_if(\(x) !is.na(x))

      dataset_name <- dataset_metadata$datasetName
      input_filename <- dataset_metadata$fileName

      # Named vector: Darwin Core name -> column name in the input file.
      vector_for_recode <-
        dataset_metadata %>%
        dplyr::select(-"datasetName", -"fileName") %>%
        unlist()

      imported_raw_dataset <- tryCatch(
        readr::read_csv(
          input_filename,
          col_types = readr::cols(.default = "c"),
          n_max = 1,
          show_col_types = FALSE
        ),
        error = \(e) exclude_dataset(dataset_name, e)
      )

      if (isFALSE(imported_raw_dataset)) {
        next
      }

      cols_not_in_input <-
        setdiff(vector_for_recode, names(imported_raw_dataset))

      if (length(cols_not_in_input) != 0) {
        message(
          "[INFO]: Column(s) ",
          paste0("`", cols_not_in_input, "`", collapse = ", "),
          " defined in the metadata not present in the ",
          dataset_name
        )
        message(
          paste(
            "[ERROR]: Column names defined in the metadata do not match",
            "column names in the", dataset_name, "file"
          )
        )
        next
      }

      save_in_filename <-
        file.path(save_in_dir, paste0("standard_", dataset_name, ".", format))

      standardized <- tryCatch(
        {
          standard_dataset <-
            readr::read_csv(
              input_filename,
              trim_ws = FALSE,
              show_col_types = FALSE
            ) %>%
            dplyr::select(dplyr::all_of(vector_for_recode)) %>%
            dplyr::mutate(database_id = paste0(dataset_name, "_", seq_len(dplyr::n()))) %>%
            dplyr::select(database_id, dplyr::everything()) %>%
            dplyr::mutate_if(is.numeric, as.character)

          message(paste("Standardizing", dataset_name, "file"))

          if (format == "qs2") {
            qs2::qs_save(standard_dataset, save_in_filename)
          } else {
            readr::write_csv(standard_dataset, save_in_filename)
          }
          TRUE
        },
        error = \(e) exclude_dataset(dataset_name, e)
      )

      if (standardized) {
        written_files <- c(written_files, save_in_filename)
      }
    }

    # Concatenate the standardized datasets written by this call
    if (format == "qs2") {
      merged_database <-
        written_files %>%
        purrr::map_dfr(
          function(x) {
            qs2::qs_read(x) %>%
              dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
          }
        )
    } else {
      merged_database <-
        written_files %>%
        purrr::map_dfr(
          \(x) readr::read_csv(x, col_types = readr::cols(.default = "c"))
        )
    }

    merged_database <-
      merged_database %>%
      dplyr::select_if(\(x) any(!is.na(x)))

    if (save_database) {
      bdc_create_dir()
      if (format == "qs2") {
        qs2::qs_save(merged_database, merged_filename)
      } else {
        readr::write_csv(merged_database, merged_filename)
      }
      message(paste("Standardized database was saved in", merged_filename))
    }

    return(merged_database)
  }

#' Check that every metadata row fills the required fields
#'
#' @param metadata The metadata data frame passed to
#'   `bdc_standardize_datasets()`.
#'
#' @return `metadata`, invisibly. Errors naming each dataset with missing
#'   (absent or `NA`) required fields and the fields it misses.
#' @noRd
bdc_check_required_metadata <- function(metadata) {
  required <- c(
    "datasetName",
    "fileName",
    "scientificName",
    "decimalLatitude",
    "decimalLongitude"
  )

  problems <- character(0)
  for (row_index in seq_len(nrow(metadata))) {
    row <- metadata[row_index, , drop = FALSE]
    is_missing <- vapply(
      required,
      \(field) !field %in% names(row) || is.na(row[[field]]),
      logical(1)
    )
    missing_fields <- required[is_missing]
    if (length(missing_fields) != 0) {
      dataset_name <-
        if ("datasetName" %in% names(row) && !is.na(row$datasetName)) {
          row$datasetName
        } else {
          paste("row", row_index)
        }
      problems <- c(
        problems,
        paste0(
          "`", dataset_name, "`: ",
          paste0("`", missing_fields, "`", collapse = ", ")
        )
      )
    }
  }

  if (length(problems) != 0) {
    stop(
      "[ERROR]: Required field(s) missing in the metadata for dataset(s) ",
      paste(problems, collapse = "; "),
      call. = FALSE
    )
  }

  invisible(metadata)
}
