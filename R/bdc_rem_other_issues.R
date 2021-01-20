
#' Title: Remove punctuation characters and digits, duplicated genus names, capitalize genus name, and substitute empty cells with NA
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_other_issues <- function(data, sci_names) {
  
  sci_names <- data[[sci_names]] %>% stringr::str_squish()

  # Remove punctuation characters, digits, and extra spaces
  res <-
    sci_names %>%
    stringr::str_replace_all(.,
                             "[[:punct:][:digit:]]",
                             " ") %>%
    stringr::str_squish()
  
  # count the number of words
  word_count <- stringr::str_count(res, "\\w+")
  
  # Capitalize the only first letter of the generic names of scientific names composed of two words
  w1 <- which(word_count == 1 | word_count == 2)
  res[w1] <- stringr::str_to_lower(res[w1])
  res[w1] <- Hmisc::capitalize(res[w1])
  
  
  # Remove duplicated generic name from names with more than three words
  w3 <- which(word_count >= 3)
  
  for (i in w3)
  {
    # split names
    u <- unlist(
      strsplit(res[i], split = " ", fixed = F, perl = T)
    )
    
    # Find duplicate generic name
    dup <- tolower(paste(unique(c(u[1], u[2])), sep = " ", collapse = " "))
    remain <- paste(u[3:length(u)], sep = " ", collapse = " ")
    p <- paste(dup, remain)
    res[i] <- p
  }
  
  res <- 
    gsub("^$", NA, res) %>% # substitute empty records by NA
    Hmisc::capitalize(.) # Capitalize first letter

  df <- 
    data.frame(res) %>% 
    rename(clean_other_issues = res) %>% 
    dplyr::bind_cols(data, .)
    
  message(
    paste(
      "bdc_rem_other_issues:\nOne collumns was added to the database.\n"))
  
  return(df)
}
