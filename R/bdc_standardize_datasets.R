#' Title: Standardize datasets columns based on metadata file
#' 
#' @param metadata a table containing information about which columns of the
#'   original dataset need to be renamed following Darwin Core terminology.
#'   Please see the `Config/DatabaseInfo.csv` file.
#'
#' @importFrom dplyr pull filter select select_if mutate n everything
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @importFrom purrr set_names
#' @importFrom readr read_csv write_csv
#'
#' @export
bdc_standardize_datasets <- function(metadata) {
  
  save_in_dir <- here::here("data", "temp")
  
  if (!fs::dir_exists(save_in_dir)) {
    fs::dir_create(save_in_dir)
  }
  
  input_file <-
    metadata %>%
    dplyr::pull(File_name_to_load)
  
  for (file_index in seq_along(input_file)) {
    
    input_filename <-
      metadata %>%
      dplyr::filter(File_name_to_load == input_file[file_index]) %>%
      pull(File_name_to_load)
    
    dataset_name <-
      metadata %>%
      dplyr::filter(File_name_to_load == input_file[file_index]) %>%
      dplyr::select(datasetName) %>%
      dplyr::pull()
    
    save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".xz")
    
    if (!file.exists(save_in_filename)) {
      
      base_names<-metadata %>%
        dplyr::filter(File_name_to_load == input_file[file_index]) %>%
        dplyr::select_if(~ !is.na(.))
      
      standard_names <-
        base_names %>%
        names(.)
      
      required<-c("datasetName", "File_name_to_load", "scientificName",
                  "decimalLatitude","decimalLongitude")
      
      if(!(required%in% standard_names %>% all)){
        stop(paste("Required field is missing from Column names defined in the metadata for", input_filename)
        )
        
      }
      
      basename_names <-base_names %>%
        dplyr::select(-datasetName, -File_name_to_load)
      
      standard_names <-
        basename_names %>%
        names(.)
      
      vector_for_recode <-
        basename_names %>%
        purrr::set_names(standard_names) %>%
        { c(.) } %>%
        unlist()
      
      imported_raw_dataset <-
        here::here(input_file[file_index]) %>%
        vroom::vroom(guess_max = 10^6, col_types = cols(.default = "c"), n_max = 1)
      
      skip_to_next <- FALSE
      
      error_message <-
        paste("[ERROR]: Column names defined in the metadata do not match column names in the", input_filename)
      
      tryCatch(
        
        if (sum(!vector_for_recode %in% names(imported_raw_dataset)) != 0) {
          
          stop(error_message)
          
        } else {
          
          standard_dataset <-
            here::here(input_file[file_index]) %>%
            vroom::vroom(guess_max = 10^6, col_types = cols(.default = "c")) %>%
            dplyr::select(all_of(vector_for_recode)) %>%
            purrr::set_names(names(vector_for_recode)) %>%
            dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
            dplyr::select(database_id, dplyr::everything())
          
          message(paste("Creating", save_in_filename))
          
          standard_dataset %>%
            vroom::vroom_write(save_in_filename)
          
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
  
}

