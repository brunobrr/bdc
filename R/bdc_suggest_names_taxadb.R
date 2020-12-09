
#' Title: Function used to standardized taxonomic names using taxadb R packpage. This functions is a modification of get.taxa function (flora package) inserted in get_names function (taxadb package) for allowing fuzzy matching.
#'
#' @param taxon 
#' @param max.distance 
#' @param provider 
#'
#' @return
#' @export
#'
#' @examples
bdc_suggest_names_taxadb <-
  function (taxon,
            max.distance = 0.75,
            provider
            
  ) {
    
    first.letter <- unique(sapply(taxon, function(i) strsplit(i, "")[[1]][1], USE.NAMES = FALSE))
   # species.first.letter <- suppressWarnings(taxadb::name_starts_with(first.letter, provider = provider))[, "scientificName"]
    species.first.letter <- taxadb::taxa_tbl(provider) %>% pull(scientificName) %>% grep(paste0("^", first.letter, collapse = "|"), .,value = TRUE)
    sug_dat <- data.frame(suggested = character(length(taxon)), distance = numeric(length(taxon)))
    
    for(i in seq_along(taxon)){
      
      sug_dat[i, ]  <- return_names(taxon[i], max.distance, species.first.letter)
      
    }
    
    sug_dat
    
    #return(sapply(taxon, FUN = return_names, max.distance, species.first.letter, USE.NAMES =FALSE))
    
  }


