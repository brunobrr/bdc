#' Create or update the column summarizing the results of data quality tests
#'
#' This function creates or updates the column ".summary" summarizing the
#' results of data quality tests (i.e., columns starting with "."). Records that
#' have failed in at least one test are flagged for further inspection (i.e.,
#' flagged as "FALSE") in the ".summary" column.
#'
#' @param data data.frame. Containing the results of data quality tests (i.e.,
#' columns starting with ".").
#' @details If existing, the column ".summary" will be removed and then updated
#' considering all test names available in the supplied database.
#'
#' @return A data.frame containing a new or an updated column ".summary".
#'
#' @importFrom dplyr select mutate contains everything bind_cols mutate_if
#' @importFrom tidyselect starts_with
#' @export
#'
#' @examples
#' .missing_names <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
#' .missing_coordinates <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
#' x <- data.frame(.missing_names, .missing_coordinates)
#'
#' bdc_summary_col(data = x)
#' 
bdc_summary_col <- function(data) {
  if (any(names(data) == ".summary")) {
    .summary <- . <- NULL

    message("Column '.summary' already exist. It will be updated")

    data <-
      data %>%
      dplyr::select(-.summary)

    df <-
      data %>%
      dplyr::select(tidyselect::starts_with(".")) %>%
      dplyr::mutate_if(is.character, ~ as.logical(as.character(.))) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)

    df <-
      dplyr::bind_cols(data, df) %>%
      dplyr::select(dplyr::everything(), .summary)
  } else {
    df <-
      data %>%
      dplyr::select(tidyselect::starts_with(".")) %>%
      dplyr::mutate_if(is.character, ~ as.logical(as.character(.))) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)

    df <- dplyr::bind_cols(data, df)
  }

  message(
    paste(
      "\nbdc_summary_col:\nFlagged",
      sum(df$.summary == FALSE),
      "records.\nOne column was added to the database.\n"
    )
  )

  return(df)
}
