#' Flag records that have prexisting quality control attributes
#'
#' This function flags records with quality control attributes provided by the
#' source (eg GBIF 'issues'). The flag is applied my matching a vector of
#' user-specified strings to a given column.
#'
#' @family prefilter
#' @param data data.frame. Containing quality control attributes.
#' @param flagStrings character. A vector of terms denoting sub-quality observations.
#' @param flagCols character string. One or more column names containing quality
#' control attributes (e.g. 'issue' for GBIF data).
#'
#' @details The function uses regex matching of multiple terms to identify
#' observations tagged with one or more of the quality control attributes. If
#' length of flagStrings is greater than one, terms are collapsed using the "|"
#' operator.
#' 
#' @return A data.frame containing the column ".flagged_by_source"
#' .Compliant (TRUE) for observations that match one or more flagStrings;
#' otherwise "FALSE".
#'
#' @importFrom dplyr if_any
#' @importFrom stringr str_c str_detect
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   issue = c("", "", "", "TAXON_MATCH_FUZZY"),
#'   occurrenceStatus = c("", "ABSENT", "", "")
#' )
#'   
#' mol_flagged_by_source(
#'   data = x, 
#'   flagStrings = c("ABSENT", "TAXON_MATCH_FUZZY"),
#'   flagCols = c("issue", "occurrenceStatus")
#' )
mol_flagged_by_source(
  data,
  flagStrings,
  flagCols
){
  
  check_col(data, flagCols)
  
  terms <- str_c(flagStrings, collapse = "|")
  
  data <- data %>%
    mutate(.flagged_by_source = if_any(dplyr::all_of(flagCols),
                                       ~str_detect(.x, terms)))
  
  message(
    paste(
      "\mol_flagged_by_source:\nFlagged",
      sum(data$.flagged_by_source == FALSE),
      "observations with flagged attributes.",
      "\nOne column was added to the database.\n"
    )
  )
  
  return(data)
}


