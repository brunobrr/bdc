
#' Title Flag records from doubtful provenance (e.g., 'fossil', 'HUCP') whose can be unreliable or unsuitable for certain analyses
#'
#' @param basisOfRecord character string. The column with basis of records
#' @param data 
#' @param names_to_keep character string. Elements of the column BasisOfRecords to keep
#'
#' @return
#' @export
#'
#' @examples
bdc_flag_xy_provenance <- function(data, basisOfRecord = "basisOfRecord", 
                                   names_to_keep = NULL) {
  
  if (is.null(names_to_keep)){
    names_to_keep <-
      c(
        "Event",
        "HUMAN_OBSERVATION",
        "HumanObservation",
        "LIVING_SPECIMEN",
        "LivingSpecimen",
        "MACHINE_OBSERVATION",
        "MachineObservation",
        "MATERIAL_SAMPLE",
        "O",
        "MaterialSample",
        "OBSERVATION",
        "Preserved Specimen",
        "PRESERVED_SPECIMEN",
        "preservedspecimen Specimen",
        "Preservedspecimen",
        "PreservedSpecimen",
        "preservedspecimen",
        "S",
        "Specimen",
        "UNKNOWN",
        "",
        NA
      )
  }
  
  data <-
    data %>%
    dplyr::mutate(.xy_provenance = .data[[basisOfRecord]] %in% names_to_keep)
  
  removed <-
    data %>%
    dplyr::filter(.xy_provenance == FALSE) %>%
    dplyr::select(.data[[basisOfRecord]]) %>% 
    dplyr::distinct()

  message(
    paste(
      "Flagged",
      sum(data$.xy_provenance == FALSE),
      "records",
      "of the following specific nature:\n",
      removed
    )
  )
  
  return(data %>% pull(.xy_provenance))
}

