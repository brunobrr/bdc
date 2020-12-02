# CHECK: 6. Invalid provenance




#' Title Flag records from doubtful provenance (e.g., 'fossil', 'HUCP') whose can be unreliable or unsuitable for certain analyses
#'
#' @param data. data.frame. Containing basis of records column.
#' @param basisOfRecord character string. The column with basis of records
#'
#' @return
#' @export
#'
#' @examples
bdc_flag_xy_provenance <- function(data, basisOfRecord) {
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

  data <-
    data %>%
    mutate(.xy_provenance = if_else(data[["basisOfRecord"]] %in% names_to_keep, TRUE, FALSE))

  removed <-
    data %>%
    dplyr::filter(.xy_provenance == FALSE) %>%
    dplyr::select(basisOfRecord) %>%
    dplyr::distinct()

  message(paste("Records of the following specific nature were flagged:", removed))

  return(data)
}
