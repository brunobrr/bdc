
#' Parse scientific names using rgnparser package
#'
#' @param data dataframe containing the collumn of scientific names to be parsed
#' @param sci_names character. Vector of scientific names
#'
#' @return
#' @export
#' @details for more details, see https://ropensci.org/technotes/2020/08/25/scientific-name-parsing/
#' @examples
bdc_gnparser <- function(data, sci_names) {

  # one-time setup to download and install rgnparser, which is used to parse scientific name (for more details, see https://github.com/ropensci/rgnparser)
  rgnparser::install_gnparser(force = F)

  data_temp <- data
  w <- which(colnames(data_temp) == sci_names)
  colnames(data_temp)[w] <- "temp"
  data_temp$id <- 1:nrow(data_temp)

  # Parse names using rgnparser
  suppressWarnings({
    gnparser <-
      data_temp %>%
      pull(temp) %>%
      rgnparser::gn_parse_tidy() %>%
      select(canonicalfull, cardinality, quality, verbatim) %>%
      rename(
        names_parsed = canonicalfull,
        temp = verbatim
      )
  })

  # Add names parsed to the full database
  df <-
    full_join(data_temp, gnparser, by = "temp") %>%
    distinct(id, .keep_all = T) %>%
    select(-c(id, temp))

  message(
    paste(
      "bdc_gnparser:\n",
      "Three collumns were added to the database.\n"
    )
  )

  return(df)
}