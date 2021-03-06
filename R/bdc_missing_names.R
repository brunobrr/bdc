#' Identify records missing scientific name
#'
#' Flags records with missing or not interpretable scientific names.
#' 
#' @param data data.frame. Containing species scientific names.
#' @param sci_names character string. The column with species scientific name.
#' Default = "scientificName".
#' @details This test identifies records missing scientific names (i.e., empty
#' or not applicable [NA] names)
#' @return A data.frame contain the column '.missing_name'. Records that have
#' failed in the test are flagged as "FALSE".
#'
#' @examples
#' 
#' x <- data.frame(scientificName = c("Ocotea odorifera", NA, "Panthera onca", ""))
#' 
#' bdc_missing_names(data = x, sci_names = "scientificName")
bdc_missing_names <- function(data, sci_names = "scientificName") {
  
  sci_names <- data[[sci_names]]
  
  sci_names <-
    sci_names %>% 
    trimws(.) %>% 
    ifelse(. == ""|. == "NA", NA, .)
  
  .missing_name <- ifelse(is.na(sci_names) == FALSE, TRUE, FALSE)
  
  df <- data.frame(data,  .missing_name)
  
  message(
    paste(
      "\nbdc_missing_names:\nFlagged",
      sum(.missing_name == FALSE),
      "records.\nOne column was added to the database.\n"))
  
  return(df)
}
