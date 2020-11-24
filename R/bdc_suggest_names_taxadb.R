
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
    taxon <- flora::fixCase(taxon)
    taxon.orig <- taxon
    uncertain <- regmatches(taxon, regexpr("[a|c]f+\\.", 
                                           taxon))
    taxon <- gsub("^\\s+|\\s+$", "", taxon)
    if (length(uncertain) != 0L) 
      taxon <- gsub("[a|c]f+\\.", "", taxon)
    ident <- regmatches(taxon, regexpr("\\s+sp\\.+\\w*", 
                                       taxon))
    if (length(ident) != 0L) 
      taxon <- unlist(strsplit(taxon, " "))[1]
    if (!nzchar(taxon)) 
      return(NA)
    
    first.letter <- strsplit(taxon, "")[[1]][1]
    species.first.letter <- suppressWarnings(taxadb::name_starts_with(first.letter, provider = provider))[, c( "taxonID", "scientificName", 
                                                                                                               "taxonRank", "taxonomicStatus",         
                                                                                                               "acceptedNameUsageID" )]
    
    
    l1 <- length(taxon)
    l2 <- length(species.first.letter$scientificName)
    out <- stringdist::stringdist(taxon, species.first.letter$scientificName)
    distance <- 1 - (out/pmax(nchar(taxon), nchar(species.first.letter$scientificName)))
    max.dist <- max(distance, na.rm = TRUE)
    if (max.dist >= max.distance) {
      if (length(ident) == 0L) {
        dis <- max(distance, na.rm = TRUE)
        res <- species.first.letter$scientificName[distance == dis][1]
        
        if (length(uncertain) == 0L) {
          
          return(c(res, dis))
        }
        else {
          res <- unlist(strsplit(res, " "))
          return(c(paste(res[1], uncertain, res[2:length(res)]), dis))
        }
      }
      else {
        paste(species.first.letter$scientificName[distance == max(distance, 
                                                                  na.rm = TRUE)][1], ident, sep = "")
      }
    }
    else {
      if (return.na) {
        c(NA, "0")
      }
      else {
        taxon.orig
      }
    }
  }


