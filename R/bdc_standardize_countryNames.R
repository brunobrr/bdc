#' Standardizes country names and gets ISO code
#'
#' Standardizes country names and gets country code information.
#'
#' @param data data.frame. Containing country names
#' @param  country character string. The column name with the country assignment
#' of each record. Default = "country".
#' 
#' @details Country names are standardized using an exact matching against a
#' list of country names in several languages from Wikipedia. If any unmatched
#' names remain,  a fuzzy matching algorithm is used to find potential
#' candidates for each misspelled countries names.
#' 
#' @return A data.frame containing two columns: country_suggested (standardized
#'   country names) and country_code (standardized iso2 country code).
#'
#' @importFrom dplyr left_join rename
#' @importFrom rnaturalearth ne_countries
#'
#' @examples
#' \dontrun{
#' country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil", "BREZIL")
#' x <- data.frame(country)
#' 
#' bdc_standardize_countryNames(
#'   data = x,
#'   country = "country")
#' }
#' 
bdc_standardize_countryNames <-
  function(data,
           country = "country") {
    
    # load auxiliary data
    message("Loading auxiliary data: country names from wikipedia\n")
    suppressMessages({
      wiki_cntr <-
        bdc_get_wiki_country() # get country names from Wikipedia
    })
    
    message("Loading auxiliary data: world map and country iso\n")
    worldmap <- bdc_get_world_map() # get world map and country iso
    
    # standardize the name of countries
    message("Standardizing country names\n")
    standard_country_names <-
      bdc_standardize_country(data = data,
                              country = country,
                              country_names_db = wiki_cntr)
    
    cntr <- "cntr_original"
    names(cntr) <- country
    data <-
      data %>%
      dplyr::left_join(standard_country_names, by = cntr)
    
    data <-
      data %>%
      dplyr::rename(
        country_suggested = cntr_suggested,
        countryCode = cntr_iso2c)
    
     w <- which(data$country != data$country_suggested)
     
    message(
      paste(
        "\nbdc_standardize_countryNames:\nThe country names of",
        length(w),
        "records were standardized.\nTwo columns were added to the database.\n"
      )
    )
    
    return(data)
  }
