#' Extract year from eventDate
#'
#' This function extracts a four-digit year from unambiguously interpretable
#' collecting dates.
#'
#' @family time
#' @param data A data frame containing a column with event date information.
#' @param eventDate Numeric or date. The column with event date information.
#'
#' @return A data.frame containing the column "year". Year information is
#' returned only if "eventDate" can be unambiguously interpretable from
#' "eventDate". Years in the future (e.g., 2050) are returned as NA as well as
#' years before 1600, which is the lower limit for collecting dates of
#' biological specimens.
#'
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract
#'
#' @export
#'
#' @examples
#' collection_date <- c(
#'   NA, "31/12/2015", "2013-06-13T00:00:00Z", "2019-05-20",
#'   "", "2013", "0001-01-00", "20", "1200"
#' )
#' x <- data.frame(collection_date)
#'
#' bdc_year_from_eventDate(data = x, eventDate = "collection_date")
#' 
bdc_year_from_eventDate <-
  function(data,
           eventDate = "eventDate") {
    col <- data[[eventDate]]

    year <-
      stringr::str_extract(col, "[[:digit:]]{4}") %>%
      as.numeric()

    res <- cbind(data, year)

    message(
      paste(
        "\nbdc_year_from_eventDate:\nFour-digit year were extracted from",
        sum(!is.na(year)),
        "records.\n"
      )
    )
    return(res)
  }
