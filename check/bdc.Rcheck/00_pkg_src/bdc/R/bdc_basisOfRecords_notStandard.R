#' Identify records from doubtful source (e.g., 'fossil', MachineObservation')
#'
#' This function flags records with an informed basis of records (i.e., the
#' records type, for example, a specimen, a human observation, or a fossil
#' specimen) not interpretable, which does not comply with Darwin Core
#' vocabulary, or unreliable or unsuitable for specific analyses.
#'
#' @family prefilter
#' @param data data.frame. Containing information about the basis of records.
#' @param basisOfRecord character string. The column name with information about
#' basis of records. Default = "basisOfRecord".
#' @param names_to_keep character string. Elements of the column BasisOfRecords
#' to keep. Default is "all", which considers a selected list of recommended
#' standard Darwin Core classes (and their spelling variations, see details).
#' By default, records missing (i.e., NA) or with "unknown" information about
#' basis of records are kept.
#'
#' @details Users are encourage to select the set of basis of records classes
#' to keep. Default = c("Event","HUMAN_OBSERVATION", "HumanObservation",
#' "LIVING_SPECIMEN", "LivingSpecimen", "MACHINE_OBSERVATION",
#' "MachineObservation", "MATERIAL_SAMPLE", "O", "Occurrence",
#' "MaterialSample", "OBSERVATION", "Preserved Specimen",
#' "PRESERVED_SPECIMEN", "preservedspecimen Specimen", "Preservedspecimen",
#' "PreservedSpecimen", "preservedspecimen", "S", "Specimen", "Taxon",
#' "UNKNOWN", "", NA)
#'
#' @return A data.frame containing the column ".basisOfRecords_notStandard"
#' .Compliant (TRUE) if 'basisOfRecord' is standard; otherwise "FALSE".
#'
#' @importFrom dplyr mutate filter select distinct
#'
#' @export
#'
#' @examples
#' x <- data.frame(basisOfRecord = c(
#'   "FOSSIL_SPECIMEN", "UNKNOWN",
#'   "RON", NA, "Specimen", "PRESERVED_SPECIMEN"
#' ))
#'
#' bdc_basisOfRecords_notStandard(
#'   data = x, 
#'   basisOfRecord = "basisOfRecord",
#'   names_to_keep = "all"
#' )
bdc_basisOfRecords_notStandard <-
  function(data,
           basisOfRecord = "basisOfRecord",
           names_to_keep = "all") {
    .data <- .basisOfRecords_notStandard <- NULL

    if (names_to_keep[1] == "all") {
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
          "Occurrence",
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
          "Taxon",
          "UNKNOWN",
          "",
          NA
        )
    }

    data <-
      data %>%
      dplyr::mutate(
        .basisOfRecords_notStandard =
          tolower(.data[[basisOfRecord]]) %in% tolower(names_to_keep)
      )

    removed <-
      data %>%
      dplyr::filter(.basisOfRecords_notStandard == FALSE) %>%
      dplyr::select(basisOfRecord) %>%
      dplyr::distinct()

    message(
      paste(
        "\nbdc_basisOfRecords_notStandard:\nFlagged",
        sum(data$.basisOfRecords_notStandard == FALSE),
        "of the following specific nature:\n",
        removed,
        "\nOne column was added to the database.\n"
      )
    )
    return(data)
  }
