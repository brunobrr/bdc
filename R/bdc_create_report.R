#' Title
#'
#' @param data a data frame
#' @param workflow_step character string containing one of the following options("prefiter", "taxonomy", "space" or "time")
#'
#' @return
#' @export
#'
#' @examples
bdc_create_report <- function(data, workflow_step) {
  suppressMessages({
    # Total number of records
    if (!file_exists("data/n_records.csv")) {
      n_records <-
        data %>%
        dplyr::summarise(n = n())
      
      data.table::fwrite(n_records, "data/n_records.csv")
      
      # Total number of records per database
      n_record_database <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::group_by(database_id) %>%
        dplyr::summarise(n_total = n())
      
      data.table::fwrite(n_record_database, "data/n_record_database.csv")
    } else {
      n_records <- read_csv("data/n_records.csv") %>% dplyr::pull(n)
      n_record_database <- read_csv("data/n_record_database.csv")
    }
    
    # Prefilter
    if (workflow_step == "prefilter") {
      pf <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t() %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(
          Perc_records_flagged =
            round((V1 / nrow(data) * 100), 2)
        ) %>%
        dplyr::rename(
          Test_name = `NA`,
          Records_flagged = V1
        )
      
      pf <-
        pf %>%
        dplyr::mutate(Description = Test_name) %>%
        dplyr::mutate(
          Description = if_else(
            Description == ".missing_name",
            "Records missing scientific name",
            Description
          ),
          Description = if_else(Description == ".missing_xy",
                                "Records missing coordinates",
                                Description
          ),
          Description = if_else(
            Description == ".invalid_xy",
            "Records with invalid coordiantes",
            Description
          ),
          Description = if_else(
            Description == ".xy_provenance",
            "Records from doubtful provenance",
            Description
          ),
          Description = if_else(
            Description == ".xy_out_country",
            "Records outside one or multiple focal countries",
            Description
          ),
          Description = if_else(Description == ".summary",
                                "Summary of all tests", Description
          )
        )
      
      pf <-
        pf %>%
        dplyr::add_row()
      
      pf <- pf %>% dplyr::select(Description, everything())
      
      pf[1 + nrow(pf), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )
      
      names(pf)[4] <- "perc_number_records(*)"
      data <- pf
    }
    
    # Taxonomy
    if (workflow_step == "taxonomy") {
      names <-
        data %>%
        group_by(notes) %>%
        summarise(number_of_records = n()) %>%
        mutate(
          perc_number_records =
            round((number_of_records / n_records) * 100, 2)
        )
      
      names$notes <- stringr::str_squish(names$notes)
      
      if (".uncer_terms" %in% names(data)) {
        taxo_unc <-
          data %>%
          dplyr::select(.uncer_terms) %>%
          dplyr::filter(.uncer_terms == FALSE) %>%
          dplyr::group_by(.uncer_terms) %>%
          dplyr::summarise(number_of_records = n()) %>%
          dplyr::mutate(
            perc_number_records =
              round((number_of_records / n_records) * 100, 2)
          ) %>%
          dplyr::rename(notes = .uncer_terms) %>%
          dplyr::mutate(notes = if_else(notes == "FALSE", "taxo_uncer", notes))
      }
      
      names <- dplyr::bind_rows(names, taxo_unc)
      
      names <-
        names %>%
        dplyr::mutate(notes = if_else(notes == "", "valid names", notes)) %>%
        dplyr::mutate(Description = notes) %>%
        dplyr::mutate(
          Description = if_else(
            Description == "taxo_uncer",
            "check: doubtful taxonomic identification",
            Description
          ),
          Description = if_else(
            is.na(Description),
            "invalid: invalid name input (i.e., NA or empty)",
            Description
          ),
          Description = if_else(Description == "accepted",
                                "valid: name accepted", Description
          ),
          Description = if_else(
            Description == "accepted | replacedSynonym",
            "valid: synonym replaced by an accepted name",
            Description
          ),
          Description = if_else(
            Description == "accepted | wasMisspelled",
            "valid: accepted name that was misspelled",
            Description
          ),
          Description = if_else(
            Description == "accepted | wasMisspelled | replacedSynonym",
            "valid: accepted names that were assigned as misspelled synonym ",
            Description
          ),
          Description = if_else(
            Description == "heterotypic synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = if_else(
            Description == "homotypic synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = if_else(
            Description == "proparte synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = if_else(
            Description == "homotypic synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = if_else(
            Description == "heterotypic synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = if_else(
            Description == "proparte synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = if_else(Description == "notFound",
                                "invalid: not found", Description
          ),
          Description = if_else(Description == "multipleAceppted",
                                "invalid: multiple accepted names found",
                                Description
          ),
          Description = if_else(
            Description == "synonym | noAcceptedName",
            "invalid: synonyms with no accepted names",
            Description
          ),
          Description = if_else(
            Description == "accepted | replacedSynonym",
            "invalid: synonyms with no accepted names",
            Description
          )
        )
      
      names <-
        names %>%
        dplyr::add_row()
      
      names <- names %>% dplyr::select(Description, everything())
      
      names[1 + nrow(names), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )
      
      names(names)[4] <- "perc_number_records(*)"
      data <- names
    }
    
    # Space
    if (workflow_step == "space") {
      space <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t() %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(
          Perc_records_flagged =
            round((V1 / n_records * 100), 2)
        ) %>%
        dplyr::rename(
          Test_name = `NA`,
          Records_flagged = V1
        )
      
      space <-
        space %>%
        dplyr::mutate(Description = Test_name) %>%
        dplyr::mutate(
          Description = if_else(Description == ".val",
                                "Coordinates valid",
                                Description
          ),
          Description = if_else(Description == ".equ",
                                "Identical coordinates",
                                Description
          ),
          Description = if_else(Description == ".zer",
                                "Plain zeros",
                                Description
          ),
          Description = if_else(Description == ".cap",
                                "Records around country capital centroid",
                                Description
          ),
          Description = if_else(Description == ".cen",
                                "Records around country or province centroids",
                                Description
          ),
          Description = if_else(Description == ".otl",
                                "Geographical outliers",
                                Description
          ),
          Description = if_else(Description == ".gbf",
                                "Records around the GBIF headquarters",
                                Description
          ),
          Description = if_else(Description == ".inst",
                                "Records around biodiversity institutions",
                                Description
          ),
          Description = if_else(Description == ".dpl",
                                "Duplicated coordinates per species",
                                Description
          ),
          Description = if_else(Description == ".rou",
                                "Rounded coordinates",
                                Description
          ),
          Description = if_else(Description == ".urb",
                                "Records withing urban areas",
                                Description
          ),
          Description = if_else(Description == ".summary",
                                "Summary of all tests", Description
          )
        )
      
      space <-
        space %>%
        dplyr::add_row()
      
      space <- space %>% dplyr::select(Description, everything())
      
      space[1 + nrow(space), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )
      
      names(space)[4] <- "perc_number_records(*)"
      data <- space
      
    }
    
    if (workflow_step == "temporal") {
      date <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t() %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(
          Perc_records_flagged =
            round((V1 / n_records * 100), 2)
        ) %>%
        dplyr::rename(
          Test_name = `NA`,
          Records_flagged = V1
        )
      
      date <-
        date %>%
        dplyr::mutate(Description = Test_name) %>%
        dplyr::mutate(
          Description = if_else(
            Description == ".year",
            "Records missing, with invalid or old date information",
            Description
          ))
      
      date <-
        date %>%
        dplyr::add_row()
      
      date <- date %>% dplyr::select(Description, everything())
      
      date[1 + nrow(date), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )
      
      names(date)[4] <- "perc_number_records(*)"
      data <- date
    }
  })
  return(data)
}