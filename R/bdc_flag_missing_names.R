#' Flag records missing scientific name
#'
#' This function add a new column `.missing_name` in the returned database
#' 
#' @param data data.frame containing a column of species names 
#' @param sci_names character string containing scientific names
#'
#' @export
#'
#' @examples
bdc_flag_missing_names <- function(data, sci_names = "scientificName") {
  
  sci_names <- data[[sci_names]]
  
  sci_names <-
    sci_names %>% 
    trimws(.) %>% 
    ifelse(. == ""|. == "NA", NA, .)
  
  .missing_name <- ifelse(is.na(sci_names) == FALSE, TRUE, FALSE)
  
  df <- data.frame(data,  .missing_name)
  
  message(
    paste(
      "\nbdc_flag_missing_names:\nFlagged",
      sum(.missing_name == FALSE),
      "records.\nOne column was added to the database.\n"))
  
  return(df)
}
