
#' Title: Extract and flag year from date
#'
#' @param x: data.frame. Containing column of event date.
#' @param column_to_test: Numeric or date. The column with event date information.
#' @param year_threshold: Numeric. Four digit year used as a threshold to flag od records. Default = NULL.
#'
#' @return
#' @export
#'
#' @examples
#' 
bdc_parse_date <- 
  function(x, 
           column_to_test, 
           year_threshold = NULL) {
    
    col <- x[[column_to_test]]
    
    year_corrected <- 
      stringr::str_extract(col, "[[:digit:]]{4}") %>% 
      as.numeric()
    
    if (is.null(year_threshold)) {
      .year_val <-
        dplyr::if_else(
          year_corrected %in% 1500:lubridate::year(Sys.Date()),
          TRUE,
          FALSE
        )
    } else if (is.numeric(year_threshold)) {
      .year_val <-
        dplyr::if_else(
          year_corrected %in% 1500:lubridate::year(Sys.Date()),
          TRUE,
          FALSE
        )
      .year_val <- .year_val & year_corrected > year_threshold
    } else {
      stop("The 'year_threshold' argument should be used with one year as a numeric data")
    }
    
    res <- cbind(x, .year_val, year_corrected)
    return(res)
  }

