#' Extract and flag year from date
#'
#' @param data A data frame containing column of event date.
#' @param col_to_test Numeric or date. The column with event date information.
#' @param year_threshold Numeric. A years threshold (four digit year) to flag records due to their age. Default = NULL.
#'
#' @importFrom dplyr if_else
#' @importFrom stringr str_extract
#' @details The function filters the column containing event dates and extract the dates with four digits. If the event dates are older than 1500, or the threshold year, they are flagged FALSE.
#' @return A data frame with the original data (x), a flagged column (.year) and a column with the extracted four digit years (year). Records with .year = FALSE means dates older than year threshold or 1500.
#' @export
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
          year %in% 1500:current_year,
          TRUE,
          FALSE
        )
    } else if (is.numeric(year_threshold)) {
      .year <-
        dplyr::if_else(
          year %in% 1500:current_year,
          TRUE,
          FALSE
        )
      .year <- .year & year > year_threshold
    } else {
      stop("The 'year_threshold' argument should be used with one year as a numeric data")
    }

    res <- cbind(data, .year, year)
    return(res)
  }

