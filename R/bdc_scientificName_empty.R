#' Identify records with empty scientific names
#'
#' Flags records with empty or not interpretable scientific names.
#'
#' @family prefilter
#' @param data data.frame. Containing the species scientific names.
#' @param sci_names character string. The column name with the species
#' scientific name. Default = "scientificName".
#'
#' @importFrom dplyr as_tibble
#' 
#' @details This test identifies records missing scientific names (i.e., empty
#' or not applicable [NA] names)
#'
#' @return A data.frame containing the column ".scientificName_empty". Compliant
#' (TRUE) if 'sci_names' is not empty; otherwise "FALSE".
#'
#' @export
#'
#' @examples
#' x <- data.frame(scientificName = c("Ocotea odorifera", NA, "Panthera onca", ""))
#' bdc_scientificName_empty(data = x, sci_names = "scientificName")
#'
bdc_scientificName_empty <-
  function(data,
           sci_names = "scientificName") {
    . <- NULL

    sci_names <- data[[sci_names]]

    sci_names <-
      sci_names %>%
      trimws(.) %>%
      ifelse(. == "" | . == "NA", NA, .)

    .scientificName_empty <- ifelse(is.na(sci_names) == FALSE, TRUE, FALSE)

    df <- data.frame(data, .scientificName_empty)

    message(
      paste(
        "\nbdc_scientificName_empty:\nFlagged",
        sum(.scientificName_empty == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(dplyr::as_tibble(df))
  }
