#' Internal function. Filter species information from local stored taxonomic
#' databases
#'
#' This function is used to harmonize scientific names through an exact matching
#' algorithm.
#' 
#' @param sci_name character vector. Containing scientific names.
#' @param db character vector. Local taxonomic database, eg. "gbif".
#' @param db_version character vector. The year of database version, eg. "2022".
#' The default is the latest version.
#'
#' @details
#'
#' The function looks for an exact match of scientific name in the local stored
#' database and returns the matched information
#' as a data.frame in Darwin Core format. Unmatched species return NA values for
#' all taxonomic information.
#'
#' @return  This function returns a data.frame in Darwin Core format with the
#' taxonomic information for the queried species.
#' @importFrom dplyr mutate sym collect right_join relocate
#' @importFrom taxadb taxa_tbl td_connect
#' @importFrom tibble tibble
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#'
#' }
bdc_filter_name <-
  function(sci_name,
           db,
           db_version = "22.12") {
    taxonID <- NULL
    # x <- tolower(sci_name)
    x <- sci_name
    
    db_tbl <-
      dplyr::mutate(
        taxadb::taxa_tbl(db,
                         schema = "dwc", db_version,
                         taxadb::td_connect()),
        input = !!dplyr::sym("scientificName")
      )
    
    species_tab <- tibble::tibble(input = x, sort = seq_along(x))
    
    out <-
      dplyr::collect(dplyr::right_join(db_tbl, species_tab, 
                                       by = "input", copy = TRUE))
    
    
    return(dplyr::relocate(out, sort, .before = taxonID))
  }

#' Internal function. Filter species information from local stored taxonomic
#' databases
#'
#' This function is used to harmonize scientific names through an exact matching
#' algorithm based on a taxonomic code (i.e., id).
#'
#' @param id character vector. Containing taxonomic code.
#' @param db character vector. Local taxonomic database, eg. "gbif".
#' @param db_version character vector. The year of database version, eg. "2022".
#' The default is the latest version.
#'
#' @details
#'
#' The function looks for an exact match of taxonomic code in the local stored
#' database and returns the matched information as a data.frame in Darwin Core
#' format. Unmatched ids return NA values for all taxonomic information.
#'
#' @return  This function returns a data.frame in Darwin Core format with the
#' taxonomic information for the queried ids.
#' @importFrom dplyr mutate collect right_join relocate sym
#' @importFrom taxadb td_connect
#' @importFrom tibble tibble
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#'
#' }
bdc_filter_id <-
  function(id, db, db_version = "22.12") {
    taxonID <- NULL
    db_tbl <-
      dplyr::mutate(
        taxadb::taxa_tbl(db,
          schema = "dwc", db_version,
          taxadb::td_connect()
        ),
        input = !!dplyr::sym("taxonID")
      )

    species_tab <- tibble::tibble(input = id, sort = seq_along(id))

    out <-
      dplyr::collect(dplyr::right_join(db_tbl, species_tab, 
                                       by = "input", copy = TRUE))

    return(dplyr::relocate(out, sort, .after = taxonID))
  }
