#' Extract and flag year from date
#'
#' @param data A data frame containing a column of the event date.
#' @param col_to_test numeric or date. The column with event date information.
#' @param year_threshold numeric. A years threshold (four-digit year) to flag
#' records due to their age. Default = NULL.
#'
#' @details The function filters the column containing event dates and extracts
#' the dates with four digits. If the event dates are older than 1600, or the
#' threshold year, they are flagged FALSE.
#'
#' @return A data frame with the original data, a flagged column (.year) and a
#' column with the extracted four-digit years (year_corrected). Records with .year = FALSE
#' means dates older than the year threshold or 1600.
#'
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract
#'
#' @export
#'
#' @examples
#' collection_date <- c(
#'   NA, "31/12/2015", "2013-06-13T00:00:00Z", "2013-06-20",
#'   "", "2013", "10-10-10", "20/05/2031", "1590"
#' )
#' x <- data.frame(collection_date)
#'
#' bdc_parse_date(data = x, col_to_test = "collection_date")
#' 
bdc_parse_date <-
  function(data,
           col_to_test,
           year_threshold = NULL) {
    current_year <- format(Sys.Date(), "%Y")

    col <- data[[col_to_test]]

    year <-
      stringr::str_extract(col, "[[:digit:]]{4}") %>%
      as.numeric()

    if (is.null(year_threshold)) {
      .year <-
        dplyr::if_else(
          year %in% 1600:current_year,
          TRUE,
          FALSE
        )
    } else if (is.numeric(year_threshold)) {
      .year <-
        dplyr::if_else(
          year %in% 1600:current_year,
          TRUE,
          FALSE
        )
      .year <- .year & year > year_threshold
    } else {
      stop("The 'year_threshold' argument should be used with one year as a numeric data")
    }

    res <- cbind(data, .year, year_corrected = year)
    return(res)
  }
