
#' Title: Countries names in different language
#'
#' @return
#' @export
#'
#' @examples
bdc_get_wiki_country <- function() {
  
  # Test if file was downloaded
  file <- here::here("Data", "countries_names", "wiki_country_names.txt")
 
  if (!fs::file_exists(file)) {
    
    # create a directory to salve the file
    save_in_dir <- here::here("Data", "countries_names")
    fs::dir_exists(save_in_dir)
    
    # Sourced from wikipedia
    URL <-
      c(
        "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(A-C)",
        "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(D-I)",
        "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(J-P)",
        "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(Q-Z)"
      )
    
    wiki_cntr <- list()
    
    for (i in 1:length(URL)) {
      temp <- URL[i] %>%
        xml2::read_html() %>%
        rvest::html_nodes("table")
      
      temp <- rvest::html_table(temp[2]) ## Just the "legend" table
      
      temp <-
        temp[[1]] %>%
        dplyr::as_tibble() %>%
        dplyr::filter(!X1 %in% LETTERS, !X1 == "English name")
      
      base::colnames(temp) <- c("english_name", "names_in_different_languages")
      wiki_cntr[[i]] <- temp
      
      # wiki_cntr[[i]] <- janitor::clean_names(temp)
      temp <-
        lapply(wiki_cntr[[i]][, 2] %>% pull(1), bdc_extract_cntr_names)
      
      names(temp) <-
        wiki_cntr[[i]] %>%
        dplyr::pull(english_name)
      
      temp <- as_tibble(plyr::ldply(temp))
      wiki_cntr[[i]] <- temp
      colnames(wiki_cntr[[i]]) <- c("english_name",  
                                    "names_in_different_languages")
      rm(list = c("temp"))
    }
    
    wiki_cntr <- dplyr::bind_rows(wiki_cntr) %>% dplyr::arrange(english_name)
    
    # Delete some names
    wiki_cntr <- wiki_cntr %>% filter(
      !wiki_cntr$english_name %in% c("Burma", "Lower Austria", "Persia"),
      !wiki_cntr$names_in_different_languages == c("Se")
    )
    
    write.table(wiki_cntr, "teste.txt", sep = "/t")
    
    wiki_cntr %>%
      vroom_write(here::here("Data", "countries_names",
                             "wiki_country_names.txt"))
    
    return(wiki_cntr)
  } else {
    wiki_cntr <-
      here::here("Data", "countries_names", "wiki_country_names.txt") %>%
      vroom::vroom()
    
    return(wiki_cntr)
  }
}

