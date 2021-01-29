
#' Title: Remove family names from species names
#'
#' @param sp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_family_names <- function(data, sci_names) {
  
  sci_names_raw <- data[[sci_names]] %>% stringr::str_squish()
  
  # Vector of names without family names
  clean_family_names <-sci_names_raw
  
  # Count number of words in order to not remove single family names
  word_count <- str_count(sci_names_raw, "\\S+")
  df <- data.frame(word_count, sci_names_raw)
  n_string <- ifelse(df[, 1] < 2, FALSE, TRUE)
  n_string <- ifelse(is.na(n_string), FALSE, n_string)
  
  posi <- which(n_string == TRUE)
  
  rem_fam <- stringr::str_remove_all(
    sci_names_raw[posi],
    regex("[a-zA-Z]+aceae|Leguminosae|Compositae",
          ignore_case = TRUE)
  )
  
  # remove extra spaces
  rem_fam <- rem_fam %>% stringr::str_squish()
  clean_family_names[posi] <- rem_fam
  .family_names <- sci_names_raw == clean_family_names
  
  df <- data.frame(.family_names, clean_family_names)
  df <- dplyr::bind_cols(data, df)
  
  message(
    paste(
      "bdc_rem_family_names:\nRemoved and flagged",
      sum(!.family_names),
      "records.\nTwo collumns were added to the database.\n"))
  
  return(df)
  
}
