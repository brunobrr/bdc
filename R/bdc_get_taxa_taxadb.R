# conferir se ha situacao onde dois nome buscados tem o mesmo nome aceito

#' Title: Get names using taxadb R package. Fuzzy match is allowed by using a modification version of taxadb get_filter names function
#'
#' @param taxa 
#' @param replace.synonyms 
#' @param suggest.names 
#' @param suggestion.distance 
#' @param parse 
#' @param db 
#'
#' @return
#' @export
#'
#' @examples
bdc_get_taxa_taxadb <-
  function (taxa,
            replace.synonyms = TRUE,
            suggest.names = TRUE,
            suggestion.distance = 0.9,
            parse = FALSE,
            db = NULL) {
    taxa <- flora::trim(taxa)
    taxa <- taxa[nzchar(taxa)]
    if (length(taxa) == 0L) 
      stop("No valid names provided.")
    col_names <- c("sort", "taxonID", "scientificName", "taxonRank", "taxonomicStatus", "acceptedNameUsageID", "kingdom", "phylum", "class" , "order",                   
                   "family", "genus", "specificEpithet", "infraspecificEpithet", "parentNameUsageID", "originalNameUsageID", 
                  "scientificNameAuthorship", "vernacularName", "input")
    
    found_name <-suppressWarnings(taxadb::filter_name(taxa, provider = db))
    found_name <- found_name[order(found_name$sort), ]
    found_name[, c("notes", "original.search", "distance")] <- rep("", nrow(found_name)) # tenho que melhorar a criaçao das colunas para as classes corretas
    found_name[, "original.search"] <- taxa
    not_found <- is.na(found_name$scientificName)
    
    if(any(not_found == TRUE)){
      if(suggest.names == TRUE){
    
        not_found_index <- which(not_found == TRUE)
        suggested_search <- sapply(taxa[not_found], FUN = bdc_suggest_names_taxadb, max.distance = suggestion.distance, provide = db) 
        suggested_name <- suggested_search[1, ]
        distance <- suggested_search[2, ]
        suggested <- !is.na(suggested_name)
        
        if(any(suggested == TRUE)){
          
          suggested_index <- not_found_index[suggested]
          suggest_data <- suppressWarnings(taxadb::filter_name(suggested_name[suggested], provider = db))
          found_name[suggested_index, colnames(suggest_data)] <- suggest_data 
          found_name[suggested_index, "notes"] <- "was misspelled"
          found_name[not_found_index, "distance"] <- as.character(round(as.numeric(distance, 2))) # corrigir para ser numeric ja de inicio
        
        }
          
        found_name[is.na(found_name$scientificName), "notes"] <- "not found"
        
      }
      else{
      
      found_name[is.na(found_name$scientificName), "notes"] <- "not found"
      
      } 
    }
    
    synonym_index <- which(found_name$taxonomicStatus =="synonym")
    nrow.synonym <- length(synonym_index)
    
    if (nrow.synonym > 0L) {
      if (replace.synonyms) {
        accepted <- suppressWarnings(taxadb::filter_id(found_name$acceptedNameUsageID[synonym_index], db))
        accepted_list <- split(accepted, as.factor(accepted$input)) 
        nrow.accepted <- sapply(accepted_list, nrow)
        accepted_empty <- sapply(accepted_list, function(i) all(is.na(i$scientificName)))
        one_accepted <- nrow.accepted == 1L
        
        if (any(one_accepted & accepted_empty == FALSE)) {
          
          replace <- synonym_index[one_accepted]
          found_name[replace, 1:18] <- accepted_list[[one_accepted]][, -1] 
          found_name[replace, "notes"] <- paste(found_name[replace, "notes"], "replaced synonym", sep = "|")
        }
          
        if(any(accepted_empty == TRUE)){
          
          found_name[accepted_empty, "notes"] <- paste(found_name[accepted_empty, "notes"], "check no accepted name", sep = "|")
            
        }
      }
          
      if (any(nrow.accepted > 1L)) {
            
        found_name[nrow.accepted > 1L, "notes"] <- paste(found_name[nrow.accepted > 1L, "notes"], "check +1 accepted", sep = "|")
            
      }
    }
  found_name[, 1] <- 1:nrow(found_name)
  found_name
  }