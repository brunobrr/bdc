#' Standardize datasets columns based on metadata file
#' 
#' @param metadata a table containing information about which columns of the
#'   original dataset need to be renamed following Darwin Core terminology.
#'   Please see the `Config/DatabaseInfo.csv` file.
#'
#' @importFrom data.table fread
#' @importFrom dplyr pull filter select select_if mutate n everything mutate_if
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom qs qsave
#' @importFrom vroom vroom cols
#'
#' @export
bdc_standardize_datasets <- function(metadata) {

fs::dir_create(here::here("Output", "Intermediate"))

merged_filename <- here::here("Output", "Intermediate", "00_merged_database.qs")

if (!file.exists(merged_filename)) {
  
  metadata <- data.table::fread(here::here("Config/DatabaseInfo.csv"))
  
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
        stop(paste("Required field is missing. Please check the columns of the", dataset_name, "in Config/DatabaseInfo"))
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
        here::here(input_file[file_index]) %>%
        vroom::vroom(guess_max = 10^6, col_types = vroom::cols(.default = "c"), n_max = 1)
      
      skip_to_next <- FALSE
      
      error_message <-
        paste("[ERROR]: Column names defined in the metadata do not match column names in the", dataset_name, "file")
      
      tryCatch(
        
        if (sum(!vector_for_recode %in% names(imported_raw_dataset)) != 0) {
          stop(error_message)
        } else {
          standard_dataset <-
            here::here(input_file[file_index]) %>%
            data.table::fread() %>%
            dplyr::select(all_of(vector_for_recode)) %>%
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
                .fun = qread,
                .progress = plyr::progress_text(char = "."), 
                .id = NULL)
  
  merged_database %>%
    mutate(database_name = str_remove(database_id, "_[0-9].*")) %>%
    distinct(database_name)
  
  waldo::compare(
    x = merged_database %>% names(),
    y = metadata %>% names()
  )
  
  merged_database<-
    merged_database %>% 
    select_if((function(x) any(!is.na(x))))
  
  merged_database %>%
    qs::qsave(merged_filename)
  
  message(paste("Merged database saved in", merged_filename))
  
} else {
  message(paste(merged_filename, "already exists!"))
}
}
