#' Creates reports on results of data quality tests
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
#' @importFrom dplyr summarise n select group_by filter mutate contains
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
#'                 .missing_names,
#'                 .missing_coordinates,
#'                 .invalid_basis_of_records,
#'                 .summary)
#' 
#' bdc_create_report(
#' data = x, 
#' database_id = "database_id",
#' workflow_step = "prefilter")
#' }
bdc_create_report <- 
  function(data, 
           databa_id = "databas_id",
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

    # A function to format the report
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
        dplyr::select(dplyr::contains(".")) %>%
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
          Description = dplyr::if_else(
            Description == ".missing_coordinates",
            "Records missing coordinates",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".invalid_coordinates",
            "Records with invalid coordiantes",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".invalid_basis_of_records",
            "Records from doubtful source",
            Description
          ),
          Description = dplyr::if_else(
            Description == ".coordinates_out_country",
            "Records outside one or multiple focal countries",
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
                         here::here("Output/Report/01_Prefilter_Report.csv"))
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
            "invalid: invalid name input (i.e., NA or empty)",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted",
            "valid: name accepted", Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | replacedSynonym",
            "valid: synonym replaced by an accepted name",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | wasMisspelled",
            "valid: accepted name that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | wasMisspelled | replacedSynonym",
            "valid: accepted names that were assigned as misspelled synonym ",
            Description
          ),
          Description = dplyr::if_else(
            Description == "heterotypic synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "homotypic synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "proparte synonym",
            "check: ambiguous synonyms linked to multiple accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "homotypic synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "heterotypic synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "proparte synonym | wasMisspelled",
            "check: ambiguous synonyms that was misspelled",
            Description
          ),
          Description = dplyr::if_else(
            Description == "notFound",
            "invalid: not found", Description
          ),
          Description = dplyr::if_else(
            Description == "multipleAceppted",
            "invalid: multiple accepted names found",
            Description
          ),
          Description = dplyr::if_else(
            Description == "synonym | noAcceptedName",
            "invalid: synonyms with no accepted names",
            Description
          ),
          Description = dplyr::if_else(
            Description == "accepted | replacedSynonym",
            "invalid: synonyms with no accepted names",
            Description
          )
        )

      res <- format_df(names)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/02_taxonomy_Report.csv"))
    }

    # Space
    if (workflow_step == "space") {
      space <-
        data %>%
        dplyr::select(dplyr::contains(".")) %>%
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
            "Records withing urban areas",
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
                         here::here("Output/Report/03_spatial_Report.csv"))
      
    }

    if (workflow_step == "temporal") {
      date <-
        data %>%
        dplyr::select(dplyr::contains(".")) %>%
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
            Description == ".year",
            "Records missing, with invalid or old date information",
            Description
          )
        )

      
      res <- format_df(date)
      data <- res[[2]]
      data.table::fwrite(res[[1]], 
                         here::here("Output/Report/04_temporal_Report.csv"))
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