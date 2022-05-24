#' Identify transposed geographic coordinates
#'
#' This function flags and corrects records when latitude and longitude appear
#' to be transposed.
#'
#' @family prefilter
#' @param data data.frame. Containing a unique identifier for each record,
#' geographical coordinates, and country names. Coordinates must be expressed
#' in decimal degrees and WGS84.
#' @param id character string. The column name with a unique record identifier.
#' Default = "database_id".
#' @param sci_names character string. The column name with species scientific
#' name. Default = "scientificName".
#' @param lat character string. The column name with latitude. Coordinates must
#' be expressed in decimal degrees and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude. Coordinates must be
#' expressed in decimal degrees and WGS84. Default = "decimalLongitude".
#' @param country character string. The column name with the country
#' assignment of each record. Default = "country".
#' @param countryCode character string. The column name with an ISO-2 country
#' code.
#' @param border_buffer numeric >= 0. A distance in decimal degrees used to
#' created a buffer around the country. Records within a given country and at
#' a specified distance from the border will be not be corrected.
#' Default = 0.2 (~22 km at the equator).
#' @param save_outputs logical. Should a table containing transposed coordinates
#' saved for further inspection? Default = FALSE.
#
#' @details This test identifies transposed coordinates resulted from mismatches
#' between the country informed for a record and coordinates. Transposed
#' coordinates often fall outside of the indicated country (i.e., in other
#' countries or in the sea). Different coordinate transformations are
#' performed to correct country/coordinates mismatches. Importantly, verbatim
#' coordinates are replaced by the corrected ones in the returned database. A
#' database containing verbatim and corrected coordinates is created in
#' "Output/Check/01_coordinates_transposed.csv" if save_outputs == TRUE. The
#' columns "country" and "countryCode" can be retrieved by using the function
#' \code{\link{bdc_country_standardized}}.
#'
#' @return A data.frame containing the column "coordinates_transposed"
#' indicating if verbatim coordinates were not transposed (TRUE). Otherwise
#' records are flagged as (FALSE) and, in this case, verbatim coordinates are
#' replaced by corrected coordinates.
#'
#' @importFrom readr write_csv
#' @importFrom dplyr tibble rename mutate select contains pull
#' @importFrom here here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' id <- c(1, 2, 3, 4)
#' scientificName <- c(
#'   "Rhinella major", "Scinax ruber",
#'   "Siparuna guianensis", "Psychotria vellosiana"
#' )
#' decimalLatitude <- c(63.43333, -14.43333, -41.90000, -46.69778)
#' decimalLongitude <- c(-17.90000, -67.91667, -13.25000, -13.82444)
#' country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil")
#'
#' x <- data.frame(
#'   id, scientificName, decimalLatitude,
#'   decimalLongitude, country
#' )
#'
#' # Get country code
#' x <- bdc_country_standardized(data = x, country = "country")
#'
#' bdc_coordinates_transposed(
#'   data = x,
#'   id = "id",
#'   sci_names = "scientificName",
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude",
#'   country = "country_suggested",
#'   countryCode = "countryCode",
#'   border_buffer = 0.2,
#'   save_outputs = FALSE 
#' ) 
#' }
#'
bdc_coordinates_transposed <-
  function(data,
           id = "database_id",
           sci_names = "scientificName",
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           country = "country",
           countryCode = "countryCode",
           border_buffer = 0.2,
           save_outputs = FALSE) {
    decimalLatitude <- decimalLongitude <- database_id <- scientificName <- NULL

    suppressWarnings({
      check_require_cran("rnaturalearth")
      check_require_cran("readr")
      check_require_github("ropensci/rnaturalearthdata")
    })

    data <- dplyr::tibble(data)
    minimum_colnames <-
      c(id, sci_names, lat, lon, country, countryCode)
    
    if (length(minimum_colnames) < 6) {
      stop("Fill all function arguments: id, sci_names, lon, lat, and
         country")
    }
    
    if (!all(minimum_colnames %in% colnames(data))) {
      stop(
        "These columns names were not found in your database: ",
        paste(minimum_colnames[!minimum_colnames %in% colnames(data)],
              collapse = ", "),
        call. = FALSE
      )
    }

    # Temporarily change names of the collumn .summary to avoid error of duplicated coordinates
    if(".summary" %in% names(data)){
      w <- which(names(data) == ".summary")
      names(data)[w] <- "temp_summary"
    }
    
    # Standardizing columns names
    data <-
      data %>%
      dplyr::rename(
        database_id = {{ id }},
        decimalLatitude = {{ lat }},
        decimalLongitude = {{ lon }},
        scientificName = {{ sci_names }},
        countryCode = {{ countryCode }}
      )

    # converts coordinates columns to numeric
    data <-
      data %>%
      dplyr::mutate(
        decimalLatitude = as.numeric(decimalLatitude),
        decimalLongitude = as.numeric(decimalLongitude)
      )

    worldmap <- bdc_get_world_map() # get world map and country iso

    # Correct latitude and longitude transposed
    message("Correcting latitude and longitude transposed\n")
    corrected_coordinates <-
      bdc_correct_coordinates(
        data = data,
        x = "decimalLongitude",
        y = "decimalLatitude",
        sp = "scientificName",
        id = "database_id",
        cntr_iso2 = "countryCode",
        world_poly = worldmap,
        world_poly_iso = "iso2c",
        border_buffer = border_buffer
      )
    
    if (!is.null(corrected_coordinates)) {
      
      # Exports a table with verbatim and transposed xy
      corrected_coordinates <-
        corrected_coordinates %>%
        dplyr::select(database_id, scientificName, dplyr::contains("decimal"))
      
      if (save_outputs) {
        bdc_create_dir()
        corrected_coordinates %>%
          readr::write_csv(here::here("Output/Check/01_coordinates_transposed.csv"))
        message(
          paste(
            "\nCheck database containing coordinates corrected in:\nOutput/Check/01_coordinates_transposed.csv"
          )
        )
      }
      
      # finding the position of records with lon/lat modified
      w <-
        which(data %>% dplyr::pull(database_id) %in% (corrected_coordinates %>% dplyr::pull(database_id)))
      
      data[w, "decimalLatitude"] <-
        corrected_coordinates[, "decimalLatitude_modified"]
      
      data[w, "decimalLongitude"] <-
        corrected_coordinates[, "decimalLongitude_modified"]
      
      # Flags transposed coordinates
      data$coordinates_transposed <- TRUE
      data[w, "coordinates_transposed"] <- FALSE
      
      # Return collumn .summary
      if("temp_summary" %in% names(data)){
        w <- which(names(data) == "temp_summary")
        names(data)[w] <- ".summary"
      }
      
      message(
        paste(
          "\nbdc_coordinates_transposed:\nCorrected",
          sum(data$coordinates_transposed == FALSE),
          "records.\nOne columns were added to the database.\n"
        )
      )
      
      return(data)
    } else{
      
      # Return collumn .summary
      if("temp_summary" %in% names(data)){
        w <- which(names(data) == "temp_summary")
        names(data)[w] <- ".summary"
      }
      
      message("No latitude and longitude were transposed\n")
      return(data)
    }
  }
