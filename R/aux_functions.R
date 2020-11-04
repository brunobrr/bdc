ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}


# Transformation of coordinates
coord_trans <- function(data, x, y, country_code, id, worldmap, worldmap_cntr_code) {
  
  data <- data %>% dplyr::select(x, y, country_code, id)
  d1 <- data.frame(x = data[, x], y = -data[, y])
  d2 <- data.frame(x = -data[, x], y = data[, y])
  d3 <- data.frame(x = -data[, x], y = -data[, y])
  d4 <- data.frame(x = data[, y], y = data[, x])
  d5 <- data.frame(x = data[, y], y = -data[, x])
  d6 <- data.frame(x = -data[, y], y = data[, x])
  d7 <- data.frame(x = -data[, y], y = -data[, x])
  
  d.list <- list(d1, d2, d3, d4, d5, d6, d7)
  rm(list = paste0('d', 1:7))
  d.list <- lapply(d.list, function(x) {
    colnames(x) <- c('x', 'y')
    return(x)
  })
  
  over_list <- list()
  
  for (d in 1:length(d.list)) {
    caluse <- sp::SpatialPoints(d.list[[d]])
    caluse@proj4string <- worldmap@proj4string
    overresult <- sp::over(caluse, worldmap)
    colnames(d.list[[d]]) <- c(paste0(x, '_modified'), paste0(y, '_modified'))
    over_list[[d]] <- data.frame(d.list[[d]], data, overresult)
    rm(caluse)
    filt <-
      which(over_list[[d]][country_code] == over_list[[d]][worldmap_cntr_code])
    if (length(filt) > 0) {
      over_list[[d]] <- over_list[[d]][filt, ]
    } else {
      over_list[[d]] <- NULL
    }
    rm(list = c('caluse', 'overresult', 'filt'))
  }
  
  rm(d.list)
  
  over_list <- over_list[!sapply(over_list <- over_list, is.null)]
  over_list <- dplyr::bind_rows(over_list)
  return(over_list)
}

# extract_cntr_names is a function to extract country names in different names from wikipedia
extract_cntr_names <- function(x){
  if(stringr::str_detect(x, 'Note')){
    x <- stringr::str_split(x, 'Note')[[1]][1]
  }
  if(stringr::str_detect(x, '[*]')){
    x <- stringr::str_split(x, '[*]')[[1]][1]
  }
  if(stringr::str_detect(x, 'Alternate, older forms')){
    x <- stringr::str_split(x, 'Alternate, older forms')[[1]][1]
  }
  x <-
    stringr::str_split(x, pattern = "[)]")[[1]] %>%
    stringr::str_split_fixed(., pattern = "[(]", n = 2)
  x <- x[, 1]
  x <-
    x %>% stringr::str_split(., pattern = ", ") %>% unlist() %>% 
    stringr::str_split(., pattern = " ,") %>% unlist() %>% 
    stringr::str_split(., pattern = ",") %>% unlist() %>% 
    stringr::str_trim() %>% 
    stringr::str_subset(., pattern = "", negate = FALSE)
  
  x2 <- x[!str_detect(x, '/|-')]
  x3.1 <- x[str_detect(x, '/')] %>%
    stringr::str_split(., pattern = "/") %>% unlist() %>% stringr::str_trim()
  x3.2 <- x[str_detect(x, '-')] %>%
    stringr::str_split(., pattern = " -", n = 2) %>% unlist() %>% stringr::str_trim()
  
  x <- c(x2, x3.1, x3.2) %>% sort %>%
    stringr::str_split(., pattern = "/") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = " -") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = "- ") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = " or ") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = "or ") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = ". ") %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_split(., pattern = '[\n]') %>% unlist() %>% stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE) %>%
    sort() %>% unique()
  
  x <- x[!(str_length(x)==1 & grepl(".", x))]
  if(any(x=='Afghanistan')){
    x <- x[-1]
  }
  x <- x %>% data.frame() %>% as_tibble()
  
  return(x) #Country name in different language 
} 


# stadard_country is a function to correct, standardize, and assign a ISO code to country names 
stadard_country <- function(data, cntry, cntry_names_db) {
  # Create a country database based on occ database
  cntr_db <-
    data %>%
    dplyr::distinct_(cntry, .keep_all = FALSE) %>%
    dplyr::arrange_(cntry) %>%
    rename(cntr_original = cntry)
  
  cntr_db$cntr_original2 <-
    stringr::str_replace_all(cntr_db$cntr_original, "[[:punct:]]", " ") %>%
    str_trim() %>% 
    stringi::stri_trans_general("Latin-ASCII") %>% 
    tolower()
  
  cntr_db <- cntr_db %>% mutate(cntr_suggested = NA)
  
  # Assign country names based on different character matching.
  cntry_names_db <-
    cntry_names_db %>% 
    mutate(names_2 = names_in_different_languages %>% 
             stringi::stri_trans_general("Latin-ASCII") %>% 
             tolower())
  
  cn <- cntry_names_db %>% dplyr::distinct(english_name) %>% pull(1)
  for (i in 1:length(cn)) {
    cntry_names_db_name <-
      cntry_names_db %>%
      dplyr::filter(english_name == cn[i]) %>% pull(names_2)
    filt <-
      which(tolower(cntr_db$cntr_original2) %in% tolower(cntry_names_db_name))
    if (length(filt) > 0) {
      message('country found: ', cn[i])
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
    cntr_db %>% mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))
  
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
      origin = 'country.name.en',
      destination = 'iso2c',
      warn = FALSE
    )
  
  cntr_db <-
    cntr_db %>%
    dplyr::select(-cntr_original2, -cntr_suggested) %>%
    dplyr::rename(cntr_suggested = cntr_suggested2)
  
  # data <- left_join(data, cntr_db, by=c('country'="cntr_original"))
  return(cntr_db)
}