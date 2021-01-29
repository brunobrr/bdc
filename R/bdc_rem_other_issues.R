
#' Title: Remove punctuation characters and digits, duplicated genus names, capitalize genus name, and substitute empty cells with NA
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_other_issues <- function(data, sci_names) {

  res <- data[[sci_names]] %>% stringr::str_squish()

  # count the number of words
  word_count <- stringr::str_count(res, "\\w+")

  # Convert to lower case and capitalize the only first letter of the generic names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae)
  w1 <- which(word_count == 1)
  res[w1] <- stringr::str_to_lower(res[w1])
  res[w1] <- Hmisc::capitalize(res[w1])

  res <-
    gsub("^$", NA, res) %>% # substitute empty records by NA
    Hmisc::capitalize(.) # Capitalize first letter

  df <-
    data.frame(res) %>%
    rename(clean_other_issues = res) %>%
    dplyr::bind_cols(data, .)

  message(
    paste(
      "bdc_rem_other_issues:\nOne collumns was added to the database.\n"
    )
  )

  return(df)
}
