#' Identify records with empty event date
#'
#' This function identifies records missing information on an event date (i.e.,
#' when a record was collected or observed).
#'
#' @family time
#' @param data A data frame containing column with event date information.
#' @param eventDate Numeric or date. The column with event date information.
#'
#' @details This test identifies records missing event date information (i.e.,
#' empty or not applicable [NA]).
#'
#' @return A data.frame containing the column ".eventDate_empty". Compliant
#' (TRUE) if 'eventDate' is not empty; otherwise "FALSE".
#'
#' @export
#'
#' @examples
#' collection_date <- c(
#'   NA, "31/12/2015", "2013-06-13T00:00:00Z", "2013-06-20",
#'   "", "2013", "0001-01-00"
#' )
#' x <- data.frame(collection_date)
#'
#' bdc_eventDate_empty(data = x, eventDate = "collection_date")
#'
bdc_eventDate_empty <-
  function(data,
           eventDate = "eventDate") {
    . <- NULL

    col <- data[[eventDate]]

    col <-
      col %>%
      trimws(.) %>%
      ifelse(. == "" | . == "NA" | . == "-" | . == "/", NA, .)

    .eventDate_empty <- ifelse(is.na(col) == FALSE, TRUE, FALSE)

    df <- data.frame(data, .eventDate_empty) %>% 
      dplyr::as_tibble()

    message(
      paste(
        "\nbdc_eventDate_empty:\nFlagged",
        sum(.eventDate_empty == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(df)
  }
