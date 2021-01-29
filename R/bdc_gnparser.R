
#' Title: Remove family names from species names
#'
#' @param sp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_gnparser <- function(data, sci_names) {
  
  sci_names <- data[[sci_names]] %>% stringr::str_squish()
  
  # Parse names using rgnparser
  gnparser <-
    rgnparser::gn_parse_tidy(sci_names) %>%
    select(cardinality, canonicalfull, quality) %>% 
    rename(names_parsed = canonicalfull)
  
  
  
  
  df <- data.frame(.family_names, clean_family_names)
  df <- dplyr::bind_cols(data, df)
  
  message(
    paste(
      "bdc_rem_family_names:\nRemoved and flagged",
      sum(!.family_names),
      "records.\nTwo collumns were added to the database.\n"))
  
  return(df)
  
}
