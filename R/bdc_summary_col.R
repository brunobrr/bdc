#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
bdc_summary_col <- function(data) {
  if (any(names(data) == ".summary")) {
    message("Column '.summary' already exist. It will be updated")
    
    data <-
      data %>%
      dplyr::select(-.summary)
    
    df <- 
      data %>%
      dplyr::select(contains(".")) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)
    
    df <- dplyr::bind_cols(data, df) %>% dplyr::select(everything(), .summary)
  } else{
    df <-
      data %>%
      dplyr::select(dplyr::contains(".")) %>%
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