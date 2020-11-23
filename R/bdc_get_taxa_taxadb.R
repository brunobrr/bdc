
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
    taxa <- trim(taxa)
    taxa <- taxa[nzchar(taxa)]
    if (length(taxa) == 0L) 
      stop("No valid names provided.")
    original.search <- taxa
    col_names <- suppressWarnings(colnames(taxadb::filter_name(NA, provider = db)))
    ncol.taxa <- length(col_names)
    res <- data.frame(matrix(vector(), length(taxa), ncol.taxa + 
                               3, dimnames = list(c(), c(col_names, "notes", "original.search", "distance"))), 
                      stringsAsFactors = FALSE)
    minus.notes <- seq_len(ncol.taxa)
    index <- 0
    for (taxon in taxa) {
      notes <- NULL
      index <- index + 1
      taxon <- fixCase(taxon)
      found <- !is.na(suppressWarnings(taxadb::get_ids(taxon, db = db)))
      
      if (!found) {
        if (suggest.names) {
          suggested <- bdc_suggest_names_taxadb(taxon, max.distance = suggestion.distance, provide = db)
          taxon <- suggested[1]
          res[index, "distance"] <- round(as.numeric(suggested[2]), 2)
        }
        else {
          res[index, "notes"] <- "not found"
          next
        }
        if (is.na(taxon)) {
          res[index, "notes"] <- "not found"
          next
        }
        else {
          notes <- "was misspelled"
        }
      }
      found_name <-suppressWarnings(taxadb::filter_name(taxon, provider = db)) 
      n_found <- sum(found_name$taxonomicStatus =="accepted")
      
      if (n_found > 0) {
        if (n_found == 1L) {
          res[index, minus.notes] <- found_name
        }
        else {
          notes <- c(notes, "check +1 accepted")
        }
        res[index, "notes"] <- paste(notes, collapse = "|")
        res[index, "original.search"] <- original.search[index]
        next
      }
      
      nrow.synonym <- sum(found_name$taxonomicStatus =="synonym")
      if (nrow.synonym > 0L) {
        if (replace.synonyms) {
          accepted <- suppressWarnings(taxadb::get_names(found_name$acceptedNameUsageID, db)) 
          nrow.accepted <- sum(!is.na(accepted))
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
            replace <- suppressWarnings(taxadb::filter_name(accepted, provider = db)) 
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
    res
  }