
#' Title: standard_country is a function to correct, standardize, and assign a ISO code to country names 
#'
#' @param data 
#' @param country 
#' @param country_names_db 
#'
#' @return
#' @export
#'
#' @examples
bdc_standardize_country <-
  function(data,
           country,
           country_names_db
  ) {
    # Create a country database based on occ database
    cntr_db <-
      data %>%
      dplyr::distinct_(country, .keep_all = FALSE) %>%
      dplyr::arrange_(country) %>%
      rename(cntr_original = country)
    
    cntr_db$cntr_original2 <-
      stringr::str_replace_all(cntr_db$cntr_original, "[[:punct:]]", " ") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower()
    
    cntr_db <- cntr_db %>% mutate(cntr_suggested = NA)
    
    # Assign country names based on different character matching.
    country_names_db <-
      country_names_db %>%
      mutate(names_2 = names_in_different_languages %>%
               stringi::stri_trans_general("Latin-ASCII") %>%
               tolower())
    
    cn <- country_names_db %>%
      dplyr::distinct(english_name) %>%
      pull(1)
    
    for (i in 1:length(cn)) {
      country_names_db_name <-
        country_names_db %>%
        dplyr::filter(english_name == cn[i]) %>%
        pull(names_2)
      
      filt <-
        which(tolower(cntr_db$cntr_original2) %in% tolower(country_names_db_name))
      
      if (length(filt) > 0) {
        message("country found: ", cn[i])
        cntr_db$cntr_suggested[filt] <- toupper(cn[i])
      }
    }
    
    cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested)] <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested)],
                                       fuzzyDist = 1,
                                       nthreads = 1)
    
    # Standardization of all names founds in cntr_suggested
    cntr_db$cntr_suggested2 <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_suggested,
                                       fuzzyDist = 1,
                                       nthreads = 1)
    cntr_db <-
      cntr_db %>% 
      mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))
    
    # Second standardization of all names cntr_original2 not
    cntr_db$cntr_suggested2[is.na(cntr_db$cntr_suggested)] <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested2)],
                                       fuzzyDist = 1,
                                       nthreads = 1)
    cntr_db <-
      cntr_db %>% mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))
    
    
    
    # Country code based on iso2c (it is possible use another code like iso3c, see ?codelist)
    cntr_db$cntr_iso2c <-
      countrycode::countrycode(
        cntr_db$cntr_suggested,
        origin = "country.name.en",
        destination = "iso2c",
        warn = FALSE
      )
    
    cntr_db <-
      cntr_db %>%
      dplyr::select(-cntr_original2, -cntr_suggested) %>%
      dplyr::rename(cntr_suggested = cntr_suggested2)
    
    return(cntr_db)
  }

