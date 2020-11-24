
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
    original.search <- taxa
    col_names <- c("sort", "taxonID", "scientificName", "taxonRank", "taxonomicStatus", "acceptedNameUsageID", "kingdom", "phylum", "class" , "order",                   
                   "family", "genus", "specificEpithet", "infraspecificEpithet", "parentNameUsageID", "originalNameUsageID", 
                  "scientificNameAuthorship", "vernacularName", "input")
    ncol.taxa <- length(col_names)
    minus.notes <- seq_len(ncol.taxa)
    #index <- 0
    
    
    #for (taxon in taxa) {
      #notes <- NULL
      #index <- index + 1
     
      # posso colocar para buscar todos os nomes encontrados e nao encontrados de uma vez
      # depois os processos sao filtrados para cada categoria
      #found <- !is.na(suppressWarnings(taxadb::get_ids(taxa, db = db))) # gargalo
    found_name <-suppressWarnings(taxadb::filter_name(taxa, provider = db))
    found_name <- found_name[found_name$sort, ]
    found_name[, c("notes", "original.search", "distance")] <- character(nrow(found_name))
    
    if(suggest.names){
    
      found <- !is.na(found_name$scientificName)
      not_found_index <- which(found == FALSE)
      suggested_name <- sapply(taxa[!found], FUN = bdc_suggest_names_taxadb, max.distance = suggestion.distance, provide = db)
      suggested <- !is.na(suggested_name[1, ])
      suggested_index <- not_found_index[suggested]
      found_name[suggested_index, 1:19] <- suppressWarnings(taxadb::filter_name(suggested_name[1, suggested], provider = db))
      found_name[is.na(found_name$scientificName), "notes"] <- "not found"
      found_name[suggested_index, "notes"] <- "was misspelled"
      found_name[suggested_index, "distance"] <- suggested_name[2, suggested_index]
    }
    else{
      
      found_name[is.na(found_name$scientificName), "notes"] <- "not found"
    } 
    
    # if (!found) {
    #    if (suggest.names) {
    #      # criar um suggest_name para multiplos taxa
    #      suggested <- bdc_suggest_names_taxadb(taxon, max.distance = suggestion.distance, provide = db) #gargalo
    #      taxon <- suggested[1]
    #      #o index vai ter que ser por cada nome nao encontrado
    #      res[index, "distance"] <- round(as.numeric(suggested[2]), 2)
        }
        #else {
        #  res[index, "notes"] <- "not found"
        #  next
        }
        #if (is.na(taxon)) {
        #  res[index, "notes"] <- "not found"
        #  next
        #}
        #else {
         # notes <- "was misspelled"
        #}
      #}
      # somente para os encontrados e e sugeridos
      #found_name <-suppressWarnings(taxadb::filter_name(taxon, provider = db)) # gargalo
      #n_found <- sum(found_name$taxonomicStatus =="accepted")
      #if (n_found > 0) {
       # if (n_found == 1L) {
        #  res[index, minus.notes] <- found_name
        #}
        #else {
        #  notes <- c(notes, "check +1 accepted")
        #}
        #res[index, "notes"] <- paste(notes, collapse = "|")
        #res[index, "original.search"] <- original.search[index]
        #next
      }
      
      synonym_index <- which(found_name$taxonomicStatus =="synonym")
      nrow.synonym <- length(synonym_index)
      # testar com mais especies para ver como fica com mais de um nome aceito
      if (nrow.synonym > 0L) {
        if (replace.synonyms) {
          accepted <- suppressWarnings(taxadb::filter_id(found_name$acceptedNameUsageID[synonym_index], db)) #gargalo 
          accepted_list <- split(accepted, as.factor(accepted$input))# split por input e testar se mais de um aceito
          nrow.accepted <- sapply(accepted_list, nrow)
          if (nrow.accepted == 0L) {
            if (nrow.synonym == 1L) {
              notes <- c(notes, "check no accepted name")
              res[index, minus.notes] <- found_name
            }
            if (nrow.synonym > 1L) {
              notes <- c(notes, "check no accepted +1 synonyms")
            }
          }
          if (nrow.accepted == 1L) {
            notes <- c(notes, "replaced synonym")
            replace <- suppressWarnings(taxadb::filter_name(accepted, provider = db)) #gargalo
            res[index, minus.notes] <- replace 
          }
          if (nrow.accepted > 1L) {
            notes <- c(notes, "check +1 accepted")
            if (nrow.synonym == 1L) {
              res[index, minus.notes] <- found_name
            }
          }
        }
        else {
          if (nrow.synonym == 1L) {
            res[index, minus.notes] <- found_name
          }
          if (nrow.synonym > 1L) {
            notes <- c(notes, "check +1 entries")
          }
        }
        res[index, "notes"] <- paste(notes, collapse = "|")
        res[index, "original.search"] <- original.search[index]
        next
      }
    }
    found_name
  }