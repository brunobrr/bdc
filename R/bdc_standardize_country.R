#' Internal function. Corrects, standardizes, and assigns a ISO code to country
#' names.
#'
#' @param data data.frame. Containing a column with country names.
#' @param country character string. A column name with country names.
#' @param country_names_db character string. A data base with candidates
#' countries names which the supplied name with be matched.
#' 
#' @importFrom dplyr distinct arrange rename mutate pull filter select left_join
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_replace_all str_trim
#' @importFrom stats na.omit
#' 
#' @noRd
#' @return Return a data.frame with original country names, suggested names and
#' ISO code to country names.
#'
#' #' @examples
#' \dontrun{
#' }
bdc_standardize_country <-
  function(data,
           country,
           country_names_db) {
    .data <- names_in_different_languages <- english_name <- lower_case <- NULL
    cntr_suggested2 <- cntr_suggested <- cntr_iso2c <- alpha2 <- alpha3 <- cntr_original2 <- NULL
    . <- NULL

    # Create a country database based on occ database
    cntr_db <-
      data %>%
      dplyr::distinct(country, .keep_all = FALSE) %>%
      dplyr::rename(cntr_original = country)
    
    cntr_db$cntr_original2 <-
      gsub("&", "and", cntr_db$cntr_original) %>% 
      stringr::str_replace_all(., "[[:punct:]]", " ") %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower()

    cntr_db <- cntr_db %>% dplyr::mutate(cntr_suggested = NA)

    # Assign country names based on different character matching.
    country_names_db <-
      country_names_db %>%
      dplyr::mutate(lower_case = names_in_different_languages %>%
                      stringr::str_replace_all(., "[[:punct:]]", " ") %>%
                      stringr::str_trim() %>%
                      stringr::str_squish() %>% 
                      stringi::stri_trans_general("Latin-ASCII") %>%
        tolower())

    cn <-
      country_names_db %>%
      dplyr::distinct(english_name) %>%
      dplyr::pull(1)

    for (i in 1:length(cn)) {
      country_names_db_name <-
        country_names_db %>%
        dplyr::filter(english_name == cn[i]) %>%
        dplyr::pull(lower_case)

      filt <-
        which(tolower(cntr_db$cntr_original2) %in% tolower(country_names_db_name))

      if (length(filt) > 0) {
        message("country found: ", cn[i])
        cntr_db$cntr_suggested[filt] <- cn[i]
      }
    }

    # Check alpha2 ISO names
    cn <-
      country_names_db %>%
      dplyr::distinct(alpha2) %>%
      dplyr::pull(1) %>% stats::na.omit()
    
    if(any(cn %in% cntr_db$cntr_original)){
      for (i in 1:length(cn)) {
        country_names_db_name <-
          country_names_db %>%
          dplyr::filter(alpha2 == cn[i]) %>%
          dplyr::pull(alpha2)
        
        filt <-
          which(tolower(cntr_db$cntr_original2) %in% tolower(country_names_db_name))
        
        
        if (length(filt) > 0) {
          cnnn <- country_names_db %>%
            dplyr::filter(alpha2 == cn[i]) %>%
            dplyr::pull(english_name)
          cnnn <- cnnn[1]
          message("country found based on iso2: ", cnnn)
          cntr_db$cntr_suggested[filt] <- cnnn
        }
      }
    }
    
    
    # Check alpha3 ISO names
    cn <-
      country_names_db %>%
      dplyr::distinct(alpha3) %>%
      dplyr::pull(1) %>% stats::na.omit()
    if(any(cn %in% cntr_db$cntr_original)){
      for (i in 1:length(cn)) {
        country_names_db_name <-
          country_names_db %>%
          dplyr::filter(alpha3 == cn[i]) %>%
          dplyr::pull(alpha3)
        
        filt <-
          which(tolower(cntr_db$cntr_original2) %in% tolower(country_names_db_name))
        
        
        if (length(filt) > 0) {
          cnnn <- country_names_db %>%
            dplyr::filter(alpha3 == cn[i]) %>%
            dplyr::pull(english_name)
          cnnn <- cnnn[1]
          message("country found based on iso3: ", cnnn)
          cntr_db$cntr_suggested[filt] <- cnnn
        }
      }
    }
    
    
    # Standardization of all names founds in cntr_suggested2 fuzzy_d 1
    cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested)] <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested)], 
                    country_names_db=country_names_db, fuzzy_d = 1) #NEW version
    
    # Standardization of all names founds in cntr_suggested2 fuzzy_d 2
    cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested)] <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested)], 
                    country_names_db=country_names_db, fuzzy_d = 2) #NEW version

    # Standardization of all names founds in cntr_suggested2 fuzzy_d 1
    cntr_db$cntr_suggested2 <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_suggested,
                    country_names_db = country_names_db,
                    fuzzy_d = 1) 
    # Standardization of all names founds in cntr_suggested2 fuzzy_d 2
    cntr_db$cntr_suggested2 <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_suggested,
                    country_names_db = country_names_db,
                    fuzzy_d = 2) 

    # Second standardization of all names cntr_original2
    cntr_db$cntr_suggested2[is.na(cntr_db$cntr_suggested2)] <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested2)],
                    country_names_db = country_names_db,
        fuzzy_d = 1)
    
    
    # Second standardization of all names cntr_original2 and FUZZY 2
    cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested2)] <-
      bdc_stdz_cntr(cntry_n = cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested2)],
                    country_names_db = country_names_db,
        fuzzy_d = 2)
    
    cntr_db <-
      cntr_db %>%
      # dplyr::mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2)) %>%
      dplyr::mutate(
        cntr_suggested = as.character(cntr_suggested),
        cntr_suggested2 = as.character(cntr_suggested2)
      )

    # Country code based on iso2c (it is possible use another code like iso3c, see ?codelist)
    country_names_db <- country_names_db %>% dplyr::select(english_name, cntr_iso2c=alpha2) %>% unique()
    cntr_db <-
      dplyr::left_join(
        cntr_db,
        country_names_db %>% dplyr::select(english_name, cntr_iso2c),
        by = c("cntr_suggested2" = "english_name")
      )
    cntr_db <-
      cntr_db %>%
      dplyr::select(-cntr_original2, -cntr_suggested) %>%
      dplyr::rename(cntr_suggested = cntr_suggested2)
    
    cntr_db[cntr_db$cntr_original %in% 0:9, 2:3] <- NA
    
    cntr_db <- cntr_db %>%
      dplyr::arrange(cntr_suggested)
    return(cntr_db)
  }
