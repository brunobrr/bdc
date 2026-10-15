#' Identify records with year out-of-range
#'
#' This function identifies records out-of-range collecting year (e.g., in the
#' future) or old records collected before a year informed in 'year_threshold'.
#'
#' @family time
#' @param data A data frame containing a column with event date information.
#' @param eventDate numeric or date. The column containing event date
#' information.
#' @param year_threshold numeric. A four-digit year threshold used to flag old
#' (potentially invalid) records. Records collected in the threshold year
#' itself are compliant. Default = 1900
#'
#' @details Following the "VALIDATION:YEAR_OUTOFRANGE"
#' \href{https://github.com/tdwg/bdq/projects/2}{Biodiversity data quality
#' group}, the results of this test are time-dependent. While the user may
#' provide a lower limit to the year, the upper limit is defined based on the
#' year when the test is run. Lower limits can be used to flag old, often
#' imprecise, records. For example, records collected before GPS advent
#' (1980). If 'year_threshold' is not provided, the lower limit to the year is
#' by default 1600, a lower limit for collecting dates of biological specimens.
#' The lower limit is inclusive: a record collected in 'year_threshold' (or in
#' 1600 when 'year_threshold' is NULL) is compliant. Records collected after
#' the current year are flagged.
#'
#' Records with empty or NA 'eventDate' are not tested and returned as TRUE
#' (compliant), whether or not 'year_threshold' is provided. Use
#' \code{\link{bdc_eventDate_empty}} to flag records with missing dates.
#'
#' @return A data.frame containing the column ".year_outOfRange". Compliant
#' (TRUE) if 'eventDate' is not out-of-range; otherwise "FALSE".
#'
#' @importFrom dplyr if_else select rename pull
#' @importFrom stringr str_extract
#'
#' @export
#'
#' @examples
#' collection_date <- c(
#'   NA, "31/12/2029", "2013-06-13T00:00:00Z", "2013-06-20",
#'   "", "2013", 1650, "0001-01-00"
#' )
#' x <- data.frame(collection_date)
#'
#' bdc_year_outOfRange(
#'   data = x,
#'   eventDate = "collection_date",
#'   year_threshold = 1900
#' )
#'
bdc_year_outOfRange <-
  function(data,
           eventDate,
           year_threshold = 1900) {
    year <- NULL

    current_year <- format(Sys.Date(), "%Y")
    col <- data[[eventDate]]
    nDigits <- function(x) nchar(trunc(abs(x)))

    suppressMessages({
      y <-
        bdc_year_from_eventDate(data.frame(col), eventDate = "col") %>%
        dplyr::select(year) %>%
        dplyr::rename(.year_outOfRange = year) %>%
        dplyr::pull()
    })

    lower_limit <- 1600

    if (!is.null(year_threshold)) {
      if (!is.numeric(year_threshold)) {
        stop("'year_threshold' is not numeric")
      }

      if (nDigits(year_threshold) != 4) {
        stop("'year_threshold' does not have four digits")
      }

      lower_limit <- max(lower_limit, year_threshold)
    }

    .year_outOfRange <-
      is.na(y) | (y >= lower_limit & y <= as.numeric(current_year))

    res <- cbind(data, .year_outOfRange)

    message(
      paste(
        "\nbdc_year_outOfRange:\nFlagged",
        sum(.year_outOfRange == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(res)
  }
