
#' Get taxa information from taxadb R package by fuzzy match.
#'This function was inspired by get.taxa function in flora R package.
#' @param sci_name A character vector of species names. The function does not clean species names (eg.: infraspecific, var., inf.), it is expected clean names.
#' The inclusion of 'var.' increases name distances and it is advised to set smaller suggestion.distance values. 
#' @param replace.synonyms A logical value (default = TRUE) whether synonyms must be replaced by the valid names found in taxadb database.
#' @param suggest.names A logical value (default = TRUE) whether species names must be suggested if it is not found in the first search. 
#' @param suggestion.distance A numeric value (default = 0.9). It is the accepted distance between the searched names and suggested ones. Distances higher than specified here are not suggested and NA is returned.  
#' @param db A character vector with the name of the database where information should be searched. The available databases are those provided by taxadb package.  
#' @param rank_name A taxonomic rank from the column named kingdom in db. This argument decreases the number of scientific names to calculate distances.  
#' @param parallel A logical value indicating whether distance calculation should be done in parallel.
#' @param ncores Number of cores to run in parallel.
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
            db = NULL, 
            rank_name = NULL, 
            rank = NULL,
            parallel = FALSE,
            ncores = 2) {
    
    # this is one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user (see ?taxadb::td_create for details).
    
    taxo_authority <- db
    taxadb::td_create(taxo_authority, schema = "dwc", overwrite = FALSE)
    
    if (any(is.na(sci_name)) | any(sci_name == "")) {
      stop("Sci_names should have taxonomic names, check for NA and empty characters such as ''.")
    }
    
    col_names <-
      c(
        "sort",
        "taxonID",
        "scientificName",
        "taxonRank",
        "taxonomicStatus",
        "acceptedNameUsageID",
        "kingdom",
        "phylum",
        "class" ,
        "order",
        "family",
        "genus",
        "specificEpithet",
        "infraspecificEpithet",
        "parentNameUsageID",
        "originalNameUsageID",
        "scientificNameAuthorship",
        "vernacularName",
        "input"
      )
    # Names found by taxadb
    found_name <-suppressWarnings(taxadb::filter_name(sci_name, provider = db))
    
    # Add new columns 
    found_name[, c("notes", "original.search", "distance")] <- tibble(notes = character(nrow(found_name)), original.search = character(nrow(found_name)), distance =  numeric(nrow(found_name)))
    
    # Flag names with more +1 accepted name
    if(nrow(found_name) != length(sci_name)){
      found_name <- bdc_clean_duplicates(found_name)
    } 
    found_name <- found_name[order(found_name$sort), ]
    found_name[, "original.search"] <- sci_name
    
    # Unresolved names
    not_found <- is.na(found_name$scientificName) & !grepl("check \\+1 accepted", found_name$notes)
    
    if(any(not_found == TRUE)){
      if(suggest.names == TRUE){
        
        not_found_index <- which(not_found == TRUE)
        
        # Searches for approximate best matches for each unresolved names based on a maximum distance
        suggested_search <-
          bdc_suggest_names_taxadb(
            sci_name[not_found_index],
            max.distance = suggestion.distance,
            provider = db,
            rank_name = rank_name,
            rank = rank,
            ncores = ncores
          ) 
        
        suggested_name <- suggested_search[, "suggested"]
        distance <- suggested_search[, "distance"]
        suggested <- !is.na(suggested_name)
        found_name[not_found_index, "distance"] <- distance
        
        # Searches valid names for each suggested names
        if(any(suggested == TRUE)){
          
          suggested_index <- not_found_index[suggested]
          
          # exclude names without suggestion
          suggested_names_filtered <- suggested_name[suggested] 
          
          # Look for data of suggested names
          suggest_data <- suppressWarnings(taxadb::filter_name(suggested_names_filtered, provider = db))
          
          # Add new columns 
          suggest_data[, c("notes", "original.search", "distance")] <-
            tibble(
              notes = character(nrow(suggest_data)),
              original.search = character(nrow(suggest_data)),
              distance =  numeric(nrow(suggest_data))
            )
          
          # Make a warning about suggestion of duplicated names (two species may have the same suggested name)
          if (any(duplicated(suggested_names_filtered))) {
            duplicated <- duplicated(suggested_names_filtered)
            
            report <-
              which(suggested_names_filtered %in% 
                      suggested_names_filtered[duplicated])
            
            names_to_check <-
              suggested_search[suggested_search[, "suggested"] %in% 
                                 suggested_names_filtered[report], 1:2]
            warning(
              "There are more than 1 sci_name with the same suggested name, please check if they are not the same species with misspelled names. Check:\n",
              paste0(
                "\n",
                "Original name -> Suggested name\n",
                paste(apply(names_to_check, 1, function(i)
                  paste(i, collapse = " -> ")), collapse = "\n")
              )
            )
            
            # Filter data of suggested names not returned by filter_name (valid duplicated names)
            duplicated_original_names <-
              suggested_search[suggested_search[, "suggested"] %in% 
                                 suggested_names_filtered[duplicated], "original"]
            
            found_name[found_name$original.search %in% duplicated_original_names, colnames(suggest_data)] <- suggest_data[suggest_data$input %in% suggested_names_filtered[duplicated],]
            
            found_name[found_name$original.search %in% duplicated_original_names,
                       "notes"] <-
              paste(found_name[found_name$original.search %in% duplicated_original_names, "notes"][[1]], "other species had the same suggested name.", sep = "|")
            
            suggest_data <-
              suggest_data[!(suggest_data$input %in% 
                               suggested_names_filtered[duplicated]),]
          }
          
          # Filter duplicated names returned by filter_name (excluding synonyms when there are valid names)
          if (any(duplicated(suggest_data$input))) {
            suggest_data <- bdc_clean_duplicates(suggest_data)
          }
          
          # Remove duplicated names from input
          suggest_data <- suggest_data %>% dplyr::distinct(input, .keep_all = T)
          
          # Saving suggested data on found_name
          suggested_original_names <- suggested_search[suggested_search[, "suggested"] %in% suggest_data$input, "original"]
          
          posi <- found_name$input %in% suggested_original_names
          found_name[posi, colnames(suggest_data)] <- suggest_data
          found_name[posi, "notes"] <- "was misspelled"
          
        } else {
          found_name[is.na(found_name$scientificName) & !grepl("check \\+1 accepted", found_name$notes), "notes"] <- "not found"
        } 
      }
      # search accepted names for synonyms
      synonym_index <- which(found_name$taxonomicStatus !="accepted")
      nrow.synonym <- length(synonym_index)
      
      if (nrow.synonym > 0L) {
        if (replace.synonyms) {
          accepted <- suppressWarnings(taxadb::filter_id
                                       (found_name$acceptedNameUsageID[synonym_index], db))
          accepted_list <- split(accepted, as.factor(accepted$input)) 
          nrow.accepted <- sapply(accepted_list, nrow)
          accepted_empty <- sapply(accepted_list,
                                   function(i) all(is.na(i$scientificName)))
          nrow.accepted[accepted_empty] <- 0L
          one_accepted <- nrow.accepted == 1L
          
          if (any(one_accepted & accepted_empty == FALSE)) {
            replace_tab <- purrr::map_dfr(accepted_list[one_accepted], 
                                          function(i)i)[, -1]
            
            replace_tab <- replace_tab[order(replace_tab$sort), ]
            p0 <- match(replace_tab$taxonID, found_name$acceptedNameUsageID)
            
            found_name <- 
              found_name %>%
              dplyr::slice(p0) %>%
              dplyr::select(-c("notes", "original.search", "distance"))
            
            found_name[p0, "notes"] <- 
              paste(found_name$notes[p0], "replaced synonym", sep = "|")
          }
          
          if(any(accepted_empty == TRUE)) {
            p <- match(names(accepted_list[accepted_empty]),
                       found_name$acceptedNameUsageID)
            
            found_name[p, "notes"] <-
              paste(found_name[p, "notes"][[1]], "check no accepted name", sep = "|")
          }
        } # close replace.synonyms
        
        if (any(nrow.accepted > 1L)) {
          p1 <- match(names(accepted_list[nrow.accepted > 1]),
                      found_name$acceptedNameUsageID)
          
          found_name[p1, "notes"] <-
            paste(found_name[p1, "notes"][[1]], "check +1 accepted", sep = "|")
        }
      }
      
      found_name[, 1] <- 1:nrow(found_name)
      found_name <- found_name[, !(colnames(found_name) == "input")]
      return(found_name)
    }
  }