#' Standardizes country names and gets country code
#'
#' This function standardizes country names and adds a new column to the
#' database containing two-letter country codes (ISO 3166-1 alpha-2).
#'
#' @family prefilter
#' @param data data.frame. Containing country names
#' @param  country character string. The column name with the country assignment
#' of each record. Default = "country".
#'
#' @details Country names are standardized using an exact matching against a
#' list of country names in several languages from International Organization for Standardization. If any unmatched
#' names remain,  a fuzzy matching algorithm is used to find potential
#' candidates for each misspelled countries names.
#'
#' @return A data.frame containing two columns: country_suggested (standardized
#' country names) and country_code (two-letter country codes; more details in
#' \href{https://github.com/stefangabos/world_countries/}{World Countries, International Organization for Standardization}).
#'
#' @importFrom dplyr left_join rename mutate if_else
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#' country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil", "BREZIL")
#' x <- data.frame(country)
#'
#' bdc_country_standardized(
#'   data = x,
#'   country = "country"
#' )
#' }
#'
bdc_country_standardized <-
  function(data,
           country = "country") {
    cntr_suggested <- cntr_iso2c <- country_suggested <- alpha3 <- english_name <- names_in_different_languages <- NULL

    if (all(colnames(data) != country)) {
      stop(
        "The column containing country names was not found. The function bdc_country_from_coordinates can be used to retrieve country names from valid geographic coordinates"
      )
    }

    suppressWarnings({
      suppressMessages({
        check_require_cran("rnaturalearth")
        check_require_github("ropensci/rnaturalearthdata")
      })
    })

    # load auxiliary data
    message("Loading auxiliary data: country names\n")
    suppressMessages({
      suppressWarnings({
        cntr_names <-
          system.file("extdata/countries_names/country_names.txt", package = "bdc") %>%
          readr::read_delim(delim = "\t") %>% # get country names
          ## FIXME 2022-10-08: There are two cases as "United States".
          dplyr::mutate(english_name = dplyr::if_else(alpha3 == "USA", "United States of America", english_name))
        cntr_names$english_name[cntr_names$english_name=="United States"] <- "United States of America"
        
        # add English names
        cntr_names2 <- cntr_names %>% 
          dplyr::select(-names_in_different_languages) %>% 
          unique %>% 
          dplyr::mutate(names_in_different_languages=english_name) %>% 
          dplyr::relocate(c("english_name", "names_in_different_languages", 
                            "alpha2", "alpha3"))
        cntr_names <- dplyr::bind_rows(cntr_names2, cntr_names)
        
        cntr_names2 <- dplyr::filter(cntr_names, english_name == "Guyana")[1, ]
        cntr_names2$names_in_different_languages <- "British guiana"
        cntr_names <- dplyr::bind_rows(cntr_names2, cntr_names)
        
        cntr_names2 <- dplyr::filter(cntr_names, english_name == "Taiwan")[1, ]
        cntr_names2$names_in_different_languages <- "Chinese Taipei"
        cntr_names <- dplyr::bind_rows(cntr_names2, cntr_names)
        
        cntr_names2 <- dplyr::filter(cntr_names, english_name == "United States Virgin Islands")[1:4, ]
        cntr_names2$names_in_different_languages <- c("U.S. Virgin Islands", "virgin islands, u.s.", "Virgin Islands (U.S.)", "Virgin Islands, U.S.")
        cntr_names <- dplyr::bind_rows(cntr_names2, cntr_names)
        
        rm(cntr_names2)
      })
    })

    # standardize the name of countries
    message("Standardizing country names\n")
    standard_country_names <-
      bdc_standardize_country(
        data = data,
        country = country,
        country_names_db = cntr_names
      )

    cntr <- "cntr_original"
    names(cntr) <- country
    data <-
      data %>%
      dplyr::left_join(standard_country_names, by = cntr)

    data <-
      data %>%
      dplyr::rename(
        country_suggested = cntr_suggested,
        countryCode = cntr_iso2c
      ) 

    w <- which(data$country != data$country_suggested)

    message(
      paste(
        "\nbdc_country_standardized:\nThe country names of",
        length(w),
        "records were standardized.\nTwo columns ('country_suggested' and 'countryCode') were added to the database.\n"
      )
    )

    return(data)
  }
