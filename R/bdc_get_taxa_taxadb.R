
#' Get taxa information from taxadb R package by fuzzy match.
#'This function was inspired by get.taxa function from flora R package.
#' @param sci_name A character vector of species names. The function does not clean species names (e.g.: infraespecific, var., inf.), it is expected clean names.
#' The inclusion of 'var.' increases name distances and it is advised to set smaller suggestion_distance values. 
#' @param replace_synonyms A logical value (default = TRUE) whether synonyms must be replaced by the valid names found in taxadb database.
#' @param suggest_names A logical value (default = TRUE) whether species names must be suggested if it is not found in the first search. 
#' @param suggestion_distance A numeric value (default = 0.9). It is the accepted distance between the searched names and suggested ones. Distances higher than specified here are not suggested and NA is returned.  
#' @param db A character vector with the name of the database where information should be searched. The available databases are those provided by taxadb package.  
#' @param rank_name A taxonomic rank from the column named kingdom in db. This argument decreases the number of scientific names to calculate distances.  
#' @param rank 
#' @param parallel A logical value indicating whether distance calculation should be done in parallel.
#' @param ncores Number of cores to run in parallel.
#' @param export_accepted A logical value (default = FALSE) whether a table is exported containing all species with more than one valid name to be further explored by the user.
#' @param Output Path to export accepted names table without the last slash (e.g., ./Output/check). The default is a package folder (~/path_to_package/Output/check).
#' @details This function matches the sci_names with the column "scientificName" from taxadb database (db argument) and returns the database information of the matched names. Not found names returns NA.
#' To increase the set of matched names, the function looks for scientific names in the database that resemble the input names by a string distance metric. First, the scientific names from the database are filtered by their taxonomic rank (rank and rank_name arguments) 
#' to avoid matches unrelated to the target taxonomy and decrease the amount of name to calculate distance. Also, scientific names are filtered by the first letter of input names to save the computational time. String distances are calculated by optimal string alignment (restricted Damerau-Levenshtein distance) that counts 
#' the number of deletions, insertions, substitutions and transpositions of adjacent characters (See \code{\link[stringdist]{stringdist}}). String distances are scaled to range from 0 to 1. The name with the most similarity and higher than suggestion_distance is returned as suggested name. The first name are returned if multiple names have the same similarity. Similarities lower than suggestion_distance return NA.
#' The rank argument indicates from which database column the rank_name should be matched. More than 1 sci_name may return the same suggested name, a warning is returned to the user to check for misspeling in the sci_names. When names are synonyms with multiple accepted names, the first accepted name is returned and a flag is included into the column named "note" of the table to enable user's evaluation afterwards. If export_accepted == TRUE a table is exported to the Output path (Default is ~/path_to_package/Output/check) with all accepted names for the species with multiple accepted names.  
#' @return This function returns a data.frame with the same number of rows and order than sci_name with the information provided by the database.
#' @export
#' @examples
#' sci_name <- c("Polystachya estrellensis" , "Tachigali rubiginosa", "Oxalis rhombeo ovata", "Axonopus canescens",
#' "Prosopis", "Guapira opposita", "Clidemia naevula", "Poincianella pyramidalis", "Hymenophyllum polyanthos")
#' test <- bdc_get_taxa_taxadb(sci_names, suggestion_distance = 0.9, db = "gbif")

bdc_get_taxa_taxadb <-
  function(sci_name,
           replace_synonyms = TRUE,
           suggest_names = TRUE,
           suggestion_distance = 0.9,
           db = "gbif",
           rank_name = NULL,
           rank = NULL,
           parallel = FALSE,
           ncores = 2,
           export_accepted = FALSE,
           Output = NULL) {
    
    # Measuring execution time
    start <- Sys.time()
    
    # This is one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user (see ?taxadb::td_create for details)
    taxo_authority <- db
    taxadb::td_create(taxo_authority, schema = "dwc", overwrite = FALSE)

    # Raw taxa names
    raw_sci_name <-
      sci_name %>%
      tibble::as_tibble() %>%
      dplyr::rename(original_search = value)

    # Only unique taxa names will be queried (empty or NA names are excluded)
    sci_name <-
      raw_sci_name %>%
      dplyr::distinct(original_search) %>% # unique taxa names
      dplyr::mutate(original_search = ifelse(original_search == "",
                                            NA, original_search)) %>%
      dplyr::filter(!is.na(original_search)) %>% # not include names NAs
      dplyr::pull(original_search)

    # Query names in taxadb (only exact match allowed)
    found_name <- suppressWarnings(taxadb::filter_name(sci_name, provider = db))

    # Save number of columns of the original search. This is important because the number of columns can vary in different databases according to the taxonomic authority chose
    ncol_tab_taxadb <- ncol(found_name)

    # Add new columns
    found_name <-
      found_name %>%
      as_tibble() %>%
      dplyr::mutate(
        notes = character(nrow(found_name)),
        original_search = character(nrow(found_name)),
        distance = numeric(nrow(found_name))
      ) %>%
      dplyr::mutate(original_search = input)

    # Flag names with more +1 found name
    if (nrow(found_name) != length(sci_name)) {
      found_name <-
        bdc_clean_duplicates(
          data = found_name,
          rank = rank,
          rank_name = rank_name
        )
    }

    found_name <- found_name[order(found_name$sort), ]

    # Unresolved names
    not_found <-
      is.na(found_name$scientificName) &
        !grepl("check \\+1 accepted", found_name$notes)

    if (any(not_found == TRUE)) {
      if (suggest_names == TRUE) {
        not_found_index <- which(not_found == TRUE)

        # Searches for approximate best matches for each unresolved names based on a maximum distance
        suggested_search <-
          bdc_suggest_names_taxadb(
            found_name$input[not_found_index],
            max_distance = suggestion_distance,
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
        if (any(suggested == TRUE)) {

          # exclude names without suggestion
          suggested_names_filtered <- suggested_name[suggested]

          # Look for data of suggested names
          suggest_data <-
            suppressWarnings(taxadb::filter_name(suggested_names_filtered,
              provider = db
            ))

          # Add new columns
          suggest_data <-
            suggest_data %>%
            as_tibble() %>%
            dplyr::mutate(
              notes = character(nrow(suggest_data)),
              original_search = character(nrow(suggest_data)),
              distance = numeric(nrow(suggest_data))
            )

          # Create a warning on different names with a same suggested name
          if (any(duplicated(suggested_names_filtered))) {
            duplicated <- duplicated(suggested_names_filtered)

            report <-
              which(suggested_names_filtered %in%
                suggested_names_filtered[duplicated])

            names_to_check <-
              suggested_search[suggested_search[, "suggested"] %in%
                suggested_names_filtered[report], 1:2]
            message(
              "There are more than 1 sci_name with the same suggested name. Please check if they are not the same species with misspelled names:\n",
              paste0(
                "\n",
                "Original name -> Suggested name\n",
                paste(apply(names_to_check, 1, function(i) {
                  paste(i, collapse = " -> ")
                }), collapse = "\n")
              )
            )

            # Filter data of suggested names not returned by filter_name (valid duplicated names)
            duplicated_original_names <-
              suggested_search[suggested_search[, "suggested"] %in%
                suggested_names_filtered[duplicated], "original"]

            p <- found_name$original_search %in% duplicated_original_names
            p_sugges <- suggest_data$input %in% 
                        suggested_names_filtered[duplicated]

            found_name[p, 1:ncol_tab_taxadb] <-
              suggest_data[p_sugges, 1:ncol_tab_taxadb]

            found_name[p, "notes"] <-
              paste(found_name[p, "notes"][[1]],
                "other species had the same suggested name",
                sep = "|"
              )

            suggest_data <-
              suggest_data[!(suggest_data$input %in%
                suggested_names_filtered[duplicated]), ]
          }

          # Filter duplicated names returned by filter_name (excluding synonyms when there are valid names)
          if (any(duplicated(suggest_data$input))) {
            suggest_data <- bdc_clean_duplicates(data = suggest_data)
          }

          suggest_data <- suggest_data[order(suggest_data$sort), ]

          # Remove duplicated names from input
          suggest_data <-
            suggest_data %>% dplyr::distinct(input, .keep_all = T)

          # Saving suggested data on found_name
          suggested_original_names <-
            suggested_search[suggested_search[, "suggested"] %in%
                               suggest_data$input, "original"]

          posi_misspelled_names <-
            which(found_name$input %in% suggested_original_names)

          found_name[posi_misspelled_names, 1:ncol_tab_taxadb] <-
            suggest_data[, 1:ncol_tab_taxadb]

          # Indicate the names replaced due to missspelling
          found_name[posi_misspelled_names, "notes"] <-
            "was misspelled"
        } else {
          # Flag not found names in the database
          found_name[is.na(found_name$scientificName) & !grepl("check \\+1 accepted", found_name$notes), "notes"] <- "not found"
        }
      } else {
        suggested_search <-
          data.frame(
            original = found_name$original_search,
            suggested = NA,
            distance = NA
          )
      }
      # search accepted names for synonyms
      synonym_index <- which(found_name$taxonomicStatus != "accepted")
      nrow_synonym <- length(synonym_index)

      if (nrow_synonym > 0L) {
        if (replace_synonyms) {
          accepted <-
            suppressWarnings(taxadb::filter_id
            (found_name$acceptedNameUsageID[synonym_index], db))

          ori_names <-
            found_name %>%
            dplyr::select(original_search) %>%
            slice(synonym_index)

          accepted <- dplyr::bind_cols(accepted, ori_names)

          # Split names by equal id
          accepted_list <- split(accepted, as.factor(accepted$input))
          nrow_accepted <- sapply(accepted_list, nrow)
          accepted_empty <- sapply(
            accepted_list,
            function(i) all(is.na(i$scientificName))
          )
          nrow_accepted[accepted_empty] <- 0L
          one_accepted <- nrow_accepted == 1L

          # Substitute synonyms by the accepted names
          if (any(one_accepted & accepted_empty == FALSE)) {
            replace_tab <- purrr::map_dfr(
              accepted_list[one_accepted],
              function(i) i
            )[, -1]

            replace_tab <- replace_tab[order(replace_tab$sort), ]
            p0 <- match(replace_tab$original_search, found_name$original_search)
            found_name[p0, colnames(replace_tab)] <- replace_tab

            found_name[p0, "notes"] <-
              paste(found_name$notes[p0], "replaced synonym", sep = "|")
          }

          # Indicate synonyms without accepted names
          if (any(accepted_empty == TRUE)) {
            p <- match(
              names(accepted_list[accepted_empty]),
              found_name$acceptedNameUsageID
            )

            found_name[p, "notes"] <-
              paste(found_name[p, "notes"][[1]], "check no accepted name", sep = "|")
          }

          # Indicate synonyms with +1 accepted name
          if (any(nrow_accepted > 1L)) {
            p1 <- match(
              names(accepted_list[nrow_accepted > 1]),
              found_name$acceptedNameUsageID
            )

            found_name[p1, "notes"] <-
              paste(found_name[p1, "notes"][[1]], "check +1 accepted", sep = "|")
          }
        } # end of replace_synonyms
      } # end of nrow_synonym
    } # end of not_found

    # Export a table with all accepted names which link to the same accepted id
    if (export_accepted == TRUE) {
      dir <- here::here("Output", "Check")
      fs::dir_create(dir)
      
      multi_accName <-
        dplyr::filter(found_name, notes == "|check +1 accepted") %>%
        dplyr::pull(., scientificName) %>%
        taxadb::filter_name(., provider = db) %>%
        dplyr::pull(., acceptedNameUsageID) %>%
        taxadb::filter_id(., db) %>%
        data.table::fwrite(
          .,
          here::here("Output/Check/02_names_multiple_accepted_names.csv")
        )
      
      message(
        "\nCheck names with more than one valid name in 'Output/Check/02_names_multiple_accepted_names.csv'\n"
      )
    }
    
    # Formatting the resulted table
    found_name <-
      suggested_search %>%
      dplyr::select(-distance) %>%
      dplyr::full_join(found_name, .,
                       by = c("original_search" = "original")) %>%
      dplyr::rename(
        suggested_name = suggested,
        original_search = original_search
      ) %>%
      dplyr::select(-c(input, sort)) %>%
      dplyr::select(taxonID:original_search, suggested_name, distance) %>%
      dplyr::mutate(distance = ifelse(distance > 0, distance, NA))

    # join  names queried to the original (complete) of names
    found_name <- dplyr::left_join(raw_sci_name, found_name, 
                                   by = "original_search")

    # Formatting table when ...
    if (replace_synonyms == F & suggest_names == F) {
      found_name[is.na(found_name$scientificName) & !grepl("check \\+1 accepted", found_name$notes), "notes"] <- "not found"

      found_name <-
        found_name %>%
        dplyr::select(-c(suggested_name, distance))
    }
    
    end <- Sys.time()
    total_time <- round(as.numeric (end - start, units = "mins"), 1)
    
    message(paste("\n", nrow(found_name), "names queried in", total_time, "minutes\n", sep = ""))
    
    return(found_name)
  }