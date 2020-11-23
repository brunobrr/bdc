##%######################################################%##
#                                                          #
####       Countries names in different language        ####
#                                                          #
##%######################################################%##

if (FALSE) {
  source("https://raw.githubusercontent.com/brunobrr/risk_assessment_flora_Brazil_I/master/R/aux_functions.R")

  ipak(
    c(
      'dplyr',
      'xml2',
      'rvest',
      'vroom'
    )
  )


  # Sourced from wikipedia

  URL <-
    c(
      "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(A-C)",
      "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(D-I)",
      "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(J-P)",
      "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(Q-Z)"
    )

  wiki_cntr <- list()

  for(i in 1:length(URL)) {
    
    temp <- URL[i] %>%
      xml2::read_html() %>%
      rvest::html_nodes("table")
    
    temp <- rvest::html_table(temp[2]) ## Just the "legend" table
    
    temp <-
      temp[[1]] %>%
      dplyr::as_tibble() %>%
      dplyr::filter(!X1 %in% LETTERS, !X1 == 'English name')
    
    base::colnames(temp) <- c('english_name', 'names_in_different_languages')
    wiki_cntr[[i]] <- temp
    
    # wiki_cntr[[i]] <- janitor::clean_names(temp)
    temp <- 
      lapply(wiki_cntr[[i]][, 2] %>%
               pull(1), extract_cntr_names)
    
    names(temp) <- 
      wiki_cntr[[i]] %>%
      dplyr::pull(english_name)
    
    temp <- as_tibble(plyr::ldply(temp))
    wiki_cntr[[i]] <- temp
    colnames(wiki_cntr[[i]])  <- c('english_name', 'names_in_different_languages')
    rm(list = c('temp'))
  }

  wiki_cntr <- dplyr::bind_rows(wiki_cntr) %>% dplyr::arrange(english_name)

  # Delete some names
  wiki_cntr <- wiki_cntr %>% filter(!wiki_cntr$english_name%in%c("Burma","Lower Austria", "Persia"), 
                       !wiki_cntr$names_in_different_languages==c("Se"))

  # wiki_cntr %>% vroom_write("wiki_country_names.xz")

}
