
#' Title: Function used to standardized taxonomic names using taxadb R packpage. This functions is a modification of get.taxa function (flora package) inserted in get_names function (taxadb package) for allowing fuzzy matching.
#'
#' @param taxon 
#' @param max.distance 
#' @param return.na 
#' @param ignore.words 
#' @param provider 
#'
#' @return
#' @export
#'
#' @examples
bdc_suggest_names_taxadb <-
  function (taxon,
            max.distance = 0.75,
            return.na = TRUE,
            ignore.words = NULL,
            provider
            
  ) {
    taxon <- sapply(taxon, flora::fixCase, USE.NAMES = FALSE)
    taxon.orig <- taxon
    uncertain <- sapply(taxon, function(i) regmatches(i, regexpr("[a|c]f+\\.", 
                                           i)))
    taxon <- gsub("^\\s+|\\s+$", "", taxon)
    uncertain_test <- sapply(uncertain, function(i) length(i) != 0L)
    if (any(uncertain_test)){ 
      taxon[uncertain_test] <- gsub("[a|c]f+\\.", "", taxon[uncertain_test])
    }
    
    ident_n <- sapply(taxon, function(i) regmatches(i, regexpr("\\s+sp\\.+\\w*", 
                                       i)))
    ident_test <- sapply(ident_n, function(i) length(i) != 0L)
    
    if (any(ident_test)){ # corrigir aqui o length(ident_n)
      taxon[ident_test] <- sapply(taxon[ident_test], function(i) unlist(strsplit(i, " "))[1], USE.NAMES = FALSE)
    }
    if (any(!nzchar(taxon))){# se algum elemento for vazio talvez separar antes para nao precisar rodar o resto
      taxon[!nzchar(taxon)] <- NA
    }
    
    first.letter <- unique(sapply(taxon, function(i) strsplit(i, "")[[1]][1], USE.NAMES = FALSE))
    
    species.first.letter <- suppressWarnings(taxadb::name_starts_with(first.letter, provider = provider))[, c( "taxonID", "scientificName", 
                                                                                                               "taxonRank", "taxonomicStatus",         
                                                                                                               "acceptedNameUsageID" )]
    
    
    #l1 <- length(taxon)
    #l2 <- length(species.first.letter$scientificName)
    out <- lapply(taxon, function(i) stringdist::stringdist(i, species.first.letter$scientificName))
    distance <- lapply(seq_along(taxon), function(i) 1 - (out[[i]]/pmax(nchar(taxon[i]), nchar(species.first.letter$scientificName))))
    max.dist <- sapply(distance, function(i) max(i, na.rm = TRUE), USE.NAMES = FALSE)
    
    suggested_names <- sapply(seq_along(taxon), function(i) return_names(max.distance, max.dist[i], distance[[i]], species.first.letter, uncertain[[i]], ident_n[[i]], return.na))
    suggested_names
  }


