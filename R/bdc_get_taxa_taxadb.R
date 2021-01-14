
#' Title: Get taxa information from taxadb R package by fuzzy match.
#'This function was inspired by get.taxa function in flora R package.
#' @param sci_name A character vector of species names. The function does not clean species names (eg.: infraspecific, var., inf.), it is expected clean names.
#' @param replace.synonyms A logical value (default = TRUE) whether synonyms must be replaced by the valid names found in taxadb database.
#' @param suggest.names A logical value (default = TRUE) whether species names must be suggested if it is not found in the first search. 
#' @param suggestion.distance A numeric value (default = 0.9). It is the accepted distance between the searched names and suggested ones. Distances higher than specified here are not suggested and NA is returned.  
#' @param db A character vector with the name of the database where information sould be searched. The available databases are those provided by taxadb package.  
#'
#' @return This function returns a data.frame with the same number of rows and order than sci_name with the information provided by the database.
#' @export
#'
#' @examples
#' sci_names <- c("Polystachya estrellensis" , "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
#' "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos")
#' test <- bdc_get_taxa_taxadb(sci_names, suggestion.distance = 0.9, db = "gbif")

bdc_get_taxa_taxadb <-
  function (sci_name,
            replace.synonyms = TRUE,
            suggest.names = TRUE,
            suggestion.distance = 0.9,
            db = NULL) {
    
    if(any(is.na(sci_name)) | any(sci_name == "")){
      stop("Sci_names should have taxonomic names, check for NA and empty characters such as ''.")
    }
   
    col_names <- c("sort", "taxonID", "scientificName", "taxonRank", "taxonomicStatus", "acceptedNameUsageID", "kingdom", "phylum", "class" , "order",                   
                   "family", "genus", "specificEpithet", "infraspecificEpithet", "parentNameUsageID", "originalNameUsageID", 
                  "scientificNameAuthorship", "vernacularName", "input")
    
    found_name <-suppressWarnings(taxadb::filter_name(sci_name, provider = db))
    found_name[, c("notes", "original.search", "distance")] <- tibble(notes = character(nrow(found_name)), original.search = character(nrow(found_name)), distance =  numeric(nrow(found_name)))
    if(nrow(found_name) != length(sci_name)){
      found_name <- bdc_clean_duplicates(found_name)
    } 
    found_name <- found_name[order(found_name$sort), ]
    found_name[, "original.search"] <- sci_name
    not_found <- is.na(found_name$scientificName) & !grepl("check \\+1 accepted", found_name$notes)
    
    if(any(not_found == TRUE)){
      if(suggest.names == TRUE){
    
        not_found_index <- which(not_found == TRUE)
        suggested_search <- bdc_suggest_names_taxadb(sci_name[not_found_index], max.distance = suggestion.distance, provide = db) 
        suggested_name <- suggested_search[, "suggested"]
        distance <- suggested_search[, "distance"]
        suggested <- !is.na(suggested_name)
        found_name[not_found_index, "distance"] <- round(as.numeric(distance, 2))
        
        if(any(suggested == TRUE)){
          
          suggested_index <- not_found_index[suggested]
          suggest_data <- suppressWarnings(taxadb::filter_name(suggested_name[suggested], provider = db))
          suggest_data[, c("notes", "original.search", "distance")] <- tibble(notes = character(nrow(suggest_data)), original.search = character(nrow(suggest_data)), distance =  numeric(nrow(suggest_data)))
          if(nrow(suggest_data) > length(suggested_index)){
          suggest_data <- bdc_clean_duplicates(suggest_data)
          }
          found_name[suggested_index, colnames(suggest_data)] <- suggest_data 
          found_name[suggested_index, "notes"] <- "was misspelled"
          
          found_name[suggested_index, "original.search"] <- suggested_search[!is.na(suggested_name), "original"]
        
        }
          
        found_name[is.na(found_name$scientificName)& !grepl("check \\+1 accepted", found_name$notes), "notes"] <- "not found"
        
      }
      else{
      
      found_name[is.na(found_name$scientificName)& !grepl("check \\+1 accepted", found_name$notes), "notes"] <- "not found"
      
      } 
    }
    
    synonym_index <- which(found_name$taxonomicStatus !="accepted")
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
          replace_tab <- purrr::map_dfr(accepted_list[one_accepted], function(i)i)[, -1]
          replace_tab <- replace_tab[order(replace_tab$sort), ]
          found_name[replace, 1:18] <- replace_tab
          found_name[replace, "notes"] <- paste(found_name$notes[replace], "replaced synonym", sep = "|")
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