#' Extract and flag year from date
#'
#' @param x: data.frame. Containing column of event date.
#' @param col_to_test: Numeric or date. The column with event date information.
#' @param year_threshold: Numeric. Four digit year used as a threshold to flag od records. Default = NULL.
#' 
#' @importFrom dplyr if_else
#' @importFrom lubridate year
#' @importFrom stringr str_extract
#' 
#' @export
#'
bdc_parse_date <-
  function(data,
           col_to_test,
           year_threshold = NULL) {

    col <- data[[col_to_test]]

    year <-
      stringr::str_extract(col, "[[:digit:]]{4}") %>%
      as.numeric()

    if (is.null(year_threshold)) {
      .year <-
        dplyr::if_else(
          year %in% 1500:lubridate::year(Sys.Date()),
          TRUE,
          FALSE
        )
    } else if (is.numeric(year_threshold)) {
      .year <-
        dplyr::if_else(
          year %in% 1500:lubridate::year(Sys.Date()),
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

