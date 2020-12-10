
#' Title: Suggests valid names based on taxadb R package. 
#' 
#' This function is a modification of suggest.name function (flora package) accessing taxadb database for fuzzy matching.
#'
#' @param sci_name A vector of scientific names to be searched in taxadb database. The function does not clean names (eg.: infraspecific names), so this procedure should be done previously.
#' @param max.distance A numeric value specifying the minimum distance between the sci_name and that found in taxadb. The default is 0.7.
#' @param provider A database where the valid name should be searched. The options are those provided by taxadb package. 
#'
#' @return This function returns a data.frame whose first column is the suggested name and the second column is the distance between the sci_name and the suggested name.   
#' @export
#'
#' @examples
#' bdc_suggest_names_taxadb(c("Cebus apela", "Puma concolar"), provider = "gbif") 
#' 
bdc_suggest_names_taxadb <-
  function (sci_name,
            max.distance = 0.75,
            provider
            
  ) {
    
    first.letter <- unique(sapply(sci_name, function(i) strsplit(i, "")[[1]][1], USE.NAMES = FALSE))
    first.letter <- base::toupper(first.letter)
    species.first.letter <- taxadb::taxa_tbl(provider) %>% pull(scientificName) %>% grep(paste0("^", first.letter, collapse = "|"), .,value = TRUE)
    sug_dat <- data.frame(suggested = character(length(sci_name)), distance = numeric(length(sci_name)))
    
    for(i in seq_along(sci_name)){
      
      sug_dat[i, ]  <- bdc_return_names(sci_name[i], max.distance, species.first.letter)
      
    }
    
    sug_dat
    
}



