#' Create reports summarizing the results of data quality tests
#'
#' @param data data.frame. Containing a unique identifier for each records and
#' the results of data quality tests.
#' @param id character string. The column name with a unique record identifier.
#' Default = "database_id".
#' @param workflow_step character string containing one of the following
#' options("prefiter", "taxonomy", "space" or "time").
#'
#' @return A data.frame containing a report summarizing the results of data
#'   quality assessment.
#' 
#' @importFrom dplyr summarise n select group_by filter mutate mutate_if
#' everything summarise_all pull rename if_else add_row bind_rows
#' @importFrom data.table fwrite
#' @importFrom tibble as_tibble
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
#' 
#' @examples
#' \dontrun{
#' database_id <- c("test_1", "test_2", "test_3", "test_4", "test_5")
#' .missing_names <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
#' .missing_coordinates <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
#' .invalid_basis_of_records <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
#' .summary <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
#' 
#' x <- data.frame(database_id,
#'                 .scientificName_empty,
#'                 .coordinates_empty,
#'                 .basisOfRrecords_notStandard,
#'                 .summary)
#' 
#' bdc_create_report(
#' data = x, 
#' database_id = "database_id",
#' workflow_step = "prefilter")
#' }
bdc_create_report <- 
  function(data, 
           database_id = "database_id",
           workflow_step) {
  suppressMessages({
    
    # Total number of records
    if (!file_exists("data/n_records.csv")) {
      n_records <-
        data %>%
        dplyr::summarise(n = dplyr::n())

      data.table::fwrite(n_records, "data/n_records.csv")

      # Total number of records per database
      n_record_database <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::group_by(database_id) %>%
        dplyr::summarise(n_total = dplyr::n())

      data.table::fwrite(n_record_database, "data/n_record_database.csv")
    } else {
      n_records <- read_csv("data/n_records.csv") %>% dplyr::pull(n)
      n_record_database <- read_csv("data/n_record_database.csv")
    }

    # Function used to formatting report
    format_df <- function(x) {
      x <-
        x %>%
        dplyr::add_row() %>%
        as.data.frame()
      
      x <- x %>% dplyr::select(Description, dplyr::everything())
      
      x[1 + nrow(x), 1] <-
        paste("(*) calculated in relation to total number of records, i.e.",
              n_records,
              "records")
      
      names(x)[4] <- "perc_number_records(*)"
      x <- x %>% replace(is.na(.), "")
      
      data <-
        x %>%
        knitr::kable(.) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover",
                                                        "condensed"))
      return(list(x, data))
    }
    
    # Prefilter
    if (workflow_step == "prefilter") {
      
      pf <-
        data %>%
        dplyr::select(dplyr::starts_with(".")) %>%
        mutate_if(is.character, ~as.logical(as.character(.))) %>% 
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
            Description == ".scientificName_empty",
            "Records with empty scientific name",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".coordinates_empty",
            "Records with empty coordinates",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".coordinates_outOfRange",
            "Records coordiantes out-of-range",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".basisOfRrecords_notStandard",
            "Records from doubtful source",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".coordinates_country_inconsistent",
            "Records outside one or multiple reference countries",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".summary",
            "Summary of all tests", Description
          )
        )
         
      res <- format_df(pf)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/01_Report_Prefilter.csv"))
  }
    # Taxonomy
    if (workflow_step == "taxonomy") {
      names <-
        data %>%
        dplyr::group_by(notes) %>%
        dplyr::summarise(number_of_records = dplyr::n()) %>%
        dplyr::mutate(
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
          dplyr::summarise(number_of_records = dplyr::n()) %>%
          dplyr::mutate(
            perc_number_records =
              round((number_of_records / n_records) * 100, 2)
          ) %>%
          dplyr::rename(notes = .uncer_terms) %>%
          dplyr::mutate(notes = if_else(notes == "FALSE", "taxo_uncer", notes))
      }

      names <- dplyr::bind_rows(names, taxo_unc)
      names$notes <- stringr::str_squish(names$notes)

      names <-
        names %>%
        dplyr::mutate(notes = if_else(notes == "", "valid names", notes)) %>%
        dplyr::mutate(Description = notes) %>%
        dplyr::mutate(
          Description = dplyr::if_else(
            Description == "taxo_uncer",
            "check: doubtful taxonomic identification",
            Description
          ),
          Description = dplyr::if_else(
            is.na(Description),
            "invalid: no interpretable name",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted",
            "valid: name accepted", Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | replaceSynonym",
            "valid: synonym replaced by an accepted name",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | wasMisspelled",
            "valid: accepted name that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | wasMisspelled | replaceSynonym",
            "valid: accepted name assigned as misspelled synonym",
            Description
          ),
          Description = dplyr::if_else(
            Description == "heterotypic synonym",
            "check: ambiguous synonym linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "homotypic synonym",
            "check: ambiguous synonym linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "proparte synonym",
            "check: ambiguous synonym linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "homotypic synonym | wasMisspelled",
            "check: ambiguous synonym that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "heterotypic synonym | wasMisspelled",
            "check: ambiguous synonym that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "proparte synonym | wasMisspelled",
            "check: ambiguous synonym that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "notFound",
            "invalid: name not found in the taxonomic database or no interpretable name", Description
          ),
          Description = dplyr::if_else(
            Description == "multipleAccepted",
            "invalid: name linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "synonym | noAcceptedName",
            "check: synonym with no accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | replacedSynonym",
            "check: synonym with no accepted names",
            Description
          )
        )

      res <- format_df(names)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/02_Report_taxonomy.csv"))
    }

    # Space
    if (workflow_step == "space") {
      space <-
        data %>%
        dplyr::select(dplyr::starts_with(".")) %>%
        mutate_if(is.character, ~as.logical(as.character(.))) %>% 
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
          Description = dplyr::if_else(
            Description == ".val",
            "Coordinates valid",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".equ",
            "Identical coordinates",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".zer",
            "Plain zeros",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".cap",
            "Records around country capital centroid",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".cen",
            "Records around country or province centroids",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".otl",
            "Geographical outliers",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".gbf",
            "Records around the GBIF headquarters",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".inst",
            "Records around biodiversity institutions",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".dpl",
            "Duplicated coordinates per species",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".rou",
            "Rounded coordinates",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".urb",
            "Records within urban areas",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".summary",
            "Summary of all tests", Description
          )
        )
      
      res <- format_df(space)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/03_Report_space.csv"))
      
    }

    if (workflow_step == "time") {
      date <-
        data %>%
        dplyr::select(dplyr::starts_with(".")) %>%
        mutate_if(is.character, ~as.logical(as.character(.))) %>% 
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
          Description = dplyr::if_else(
            Description == ".eventDate_empty",
            "Records with empty eventDate",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".year",
            "Records with year out-of-range (i.e., year in the future or collected before a threshold defined by the user).",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".summary",
            "Summary of all tests", Description
          )
        )

      
      res <- format_df(date)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/04_Report_time.csv"))
    }
  })
  
  message(
    paste(
      "\nbdc_create_report:\nCheck the report summarizing the results of the",
      workflow_step, 
      "in:\nOutput/Report\n"
    )
  )
  return(data)
}