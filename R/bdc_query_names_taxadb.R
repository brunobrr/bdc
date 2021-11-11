#' Matching scientific names against local stored taxonomic databases
#'
#' Standardization of scientific names by correcting spelling errors and
#' converting nomenclatural synonyms to currently accepted names. Names are
#' standardized based on one out of ten taxonomic authorities (i.e., backbones)
#' available in the taxadb package. Spelling errors are corrected using a fuzzy
#' match algorithm based on a match distance defined by the user. The name
#' standardization quality can be accessed in the column “notes” placed in the
#' table resulting from the name standardization process.
#'
#' @family {taxonomy}
#' @param sci_name character string. Containing scientific names to be queried.
#' @param replace_synonyms logical. Should synonyms be replaced by accepted
#' names? Default = TRUE.
#' @param suggest_names logical. Tries to find potential candidate names for
#' misspelled names not resolved by an exact match. Default = TRUE.
#' @param suggestion_distance numeric. A threshold value determining the
#' acceptable orthographical distance between searched and candidate names.
#' Names with matching distance value lower threshold informed are returned as
#' NA. Default = 0.9.
#' @param db character string. The name of the taxonomic authority database to
#' be used in the taxonomic standardization process. Default = "gbif".
#' Use "all" to install all available taxonomic databases automatically.
#' @param rank_name character string. A taxonomic rank used to filter the
#' taxonomic database. Options available are: "kingdom", "phylum", "class",
#' "order", "family", and "genus".
#' @param rank character string. Taxonomic rank name (e.g. "Plantae",
#' "Animalia", "Aves", "Carnivora". Default is NULL.
#' @param parallel logical. Should a parallelization process be used?
#' Default=FALSE
#' @param ncores numeric. The number of cores to run in parallel.
#' @param export_accepted logical. Determines if a table containing all records
#' with names linked to multiple accepted names must be saved for further
#' inspection. Default = FALSE.
#' @details
#'
#' ## Taxadb databases
#'
#' The taxonomic harmonization is based upon one taxonomic authority database.
#' The following taxonomic databases are available in taxadb package are:
#'
#' * **itis**: Integrated Taxonomic Information System
#' * **ncbi**: National Center for Biotechnology Information
#' * **col**: Catalogue of Life
#' * **tpl**: The Plant List
#' * **gbif**: Global Biodiversity Information Facility
#' * **fb**: FishBase
#' * **slb**: SeaLifeBase
#' * **wd**: Wikidata
#' * **ott**: OpenTree Taxonomy
#' * **iucn**: International Union for Conservation of Nature
#'
#'The bdc_query_names_taxadb processes as this:
#'
#' **Creation of a local taxonomic database**
#'
#' This is a one-time setup used to download, extract, and import one or all
#' (n=10)  taxonomic databases specified in the argument "db". The downloading
#' process may take a few minutes depending on your connection and database
#' size. By default, the "gbif" database following a Darwin Core schema is
#' installed. (see ?taxadb::td_create for details).
#'
#' **Taxonomic harmonization**
#'
#' The taxonomic harmonization is divided into two distinct phases according to
#' the matching type to be undertaken.
#'
#' **Exact matching**
#'
#' Firstly, the algorithm attempts to find an exact matching
#' for each original scientific name supplied using the function "filter_name"
#' from taxadb package. If an exact matching cannot be found, names are returned
#' as Not Available (NA). Also, it is possible that a scientific name match
#' multiple accepted names. In such cases, the "bdc_clean_duplicates" function
#' is used to flag and remove names with multiple accepted names.
#'
#' Information on higher taxa (e.g., kingdom or phylum) can be used to
#' disambiguate names linked to multiple accepted names. For example, the genus
#' "Casearia" is present in both Animalia and Plantae kingdoms. When handling
#' names of Plantae, it would be helpful to get rid of names belonging to the
#' Animalia to avoid flagging "Caseria" as having multiple accepted names.
#' Following Norman et al. (2020), such cases are left to be fixed by the user.
#' If "export_accepted" = TRUE a database containing a list of all records with
#' names linked to multiple accepted names is saved in the "Output" folder.
#'
#' **Fuzzy matching**
#'
#' Fuzzy matching will be applied when "suggest_names" is TRUE and only for
#' names not resolved by an exact match. In such cases, a fuzzy matching
#' algorithm processes name-matching queries to find a potential matching
#' candidate from the specified taxonomic database. Fuzzy matching identifies
#' probable names (here identified as suggested names) for original names via a
#' measure of orthographic similarity (i.e., distance). Orthographic distance is
#' calculated by optimal string alignment (restricted Damerau-Levenshtein
#' distance) that counts the number of deletions, insertions, substitutions, and
#' adjacent characters' transpositions. It ranges from 0 to 1, being 1 an
#' indicative of a perfect match. A threshold distance, i.e. the lower value of
#' match acceptable, can be informed by user (in the "suggest_distance"
#' argument). If the distance of a candidate name is equal or higher than the
#' distance informed by user, the candidate name is returned as suggested name.
#' Otherwise, names are returned as NA.
#'
#' To increase the probability of finding a potential match candidate and to
#' save time, two steps are taken before conducting fuzzy matching. First, if
#' supplied, information on higher taxon (e.g., kingdom, family) is used to
#' filter the taxonomic database. This step removes matching ambiguity by
#' avoiding matching names from unrelated taxonomic ranks (e.g., match a plant
#' species against a taxonomic database containing animal names) and decreases
#' the number of names in the taxonomic database used to calculate the matching
#' distance. Then, the taxonomic database is filtered according to a set of
#' firsts letters of all input names. This process reduces the number of names
#' in the taxonomic database to which each original name should be compared When
#' a same suggested name is returned for different input names, a warning is
#' returned asking users to check whether the suggested name is valid.
#'
#' **Report**
#'
#' The name standardization processes' quality can be accessed in the column
#' "notes" placed in the table resulting from the name standardization process.
#' The column "notes" contains assertions on the name standardization process
#' based on Carvalho (2017). The notes can be grouped in two categories:
#' accepted names and those with a taxonomic issue or warning, needing further
#' inspections. Accepted names can be returned as "accepted" (valid accepted
#' name), "replaceSynonym" (a synonym replaced by an accepted name),
#' "wasMisspelled" (original name was misspelled), "wasMisspelled |
#' replaceSynonym" (misspelled synonym replaced by an accepted name), and
#' "synonym" (original names is a synonym without accepted names in the
#' database). Similarly, the following notes are used to flag taxonomic issues:
#' "notFound" (no matching name found), "multipleAccepted" (name with multiple
#' accepted names), "noAcceptedName" (no accepted name found), and ambiguous
#' synonyms such as "heterotypic synonym", "homotypic synonym", and "pro-parte
#' synonym". Ambiguous synonyms, names that have been published more than once
#' describing different species, have more than one accepted name and cannot be
#' resolved. Such cases are flagged and left to be determined by the user.

#' @return This function returns data.frame containing the results of the
#' taxonomic harmonization process. The database is returned in the same order
#' of sci_name.
#'
#' @importFrom fs dir_create
#' @importFrom here here
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect str_squish
#' @importFrom taxadb td_create filter_name filter_id
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sci_name <- c(
#'     "Polystachya estrellensis",
#'     "Tachigali rubiginosa",
#'     "Oxalis rhombeo ovata",
#'     "Axonopus canescens",
#'     "Prosopis",
#'     "Guapira opposita",
#'     "Clidemia naevula",
#'     "Poincianella pyramidalis",
#'     "Hymenophyllum polyanthos")
#'
#' names_harmonization <-
#'   bdc_query_names_taxadb(
#'   sci_name,
#'   replace_synonyms = TRUE,
#'   suggest_names = TRUE,
#'   suggestion_distance = 0.9,
#'   db = "gbif",
#'   parallel = TRUE,
#'   ncores = 2,
#'   export_accepted = FALSE)
#' }
#'
bdc_query_names_taxadb <-
  function(sci_name,
           replace_synonyms = TRUE,
           suggest_names = TRUE,
           suggestion_distance = 0.9,
           db = "gbif",
           rank_name = NULL,
           rank = NULL,
           parallel = FALSE,
           ncores = 2,
           export_accepted = FALSE) {

    # Create a directory to save the result
    bdc::bdc_create_dir()

    # FIXME: set a env var for now
    # REVIEW: https://github.com/ropensci/taxadb/issues/91
    Sys.setenv("CONTENTID_REGISTRIES" = "https://hash-archive.carlboettiger.info")
    Sys.setenv("TAXADB_DRIVER"="MonetDBLite")

    # Measuring the execution time
    start <- Sys.time()

    # This is one-time setup used to download, extract and import taxonomic
    # database from the taxonomic authority defined by the user (see
    # ?taxadb::td_create for details)
    taxadb::td_create(provider = db, schema = "dwc", overwrite = FALSE)

    # Raw taxa names
    raw_sci_name <-
      sci_name %>%
      tibble::as_tibble() %>%
      dplyr::rename(original_search = value)

    # Only unique names will be queried (empty or NA names are temporarily
    # excluded)
    sci_name <-
      raw_sci_name %>%
      dplyr::distinct(original_search) %>% # unique taxa names
      dplyr::mutate(original_search = ifelse(original_search == "",
                                             NA, original_search)) %>%
      dplyr::filter(!is.na(original_search)) %>%
      dplyr::pull(original_search)

    # Querying names using 'taxadb' (only EXACT match allowed)
    found_name <- suppressWarnings(taxadb::filter_name(sci_name, provider = db))

    # Create a vector containing the number of columns of the taxonomic
    # database. This is important because the number of columns varies according
    # to the taxonomic authority selected.
    ncol_tab_taxadb <- ncol(found_name)

    # Add three new columns (notes, original_search, and distance).
    # notes: save results of the taxonomic standardization process;
    # original_search: contains original names queried;
    # distance: values of orthographic distance between original and suggested
    # names (calculated when fuzzy match is applied).
    found_name <-
      found_name %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        notes = character(nrow(found_name)),
        original_search = character(nrow(found_name)),
        distance = numeric(nrow(found_name))
      ) %>%
      dplyr::mutate(original_search = input)

    # Flag names with names with multiple accepted names
    if (nrow(found_name) != length(sci_name)) {
      found_name <-
        bdc_clean_duplicates(data = found_name,
                             rank = rank,
                             rank_name = rank_name)
    }

    # Reordering the database
    found_name <- found_name[order(found_name$sort), ]

    # Selects unresolved names. Such names are queried using FUZZY match if
    # suggest_names is TRUE.
    not_found <-
      is.na(found_name$scientificName) &
      !grepl("multipleAccepted", found_name$notes)

    if (any(not_found == TRUE)) {
      if (suggest_names == TRUE) {
        not_found_index <- which(not_found == TRUE)

        # Searches for best matches to each unresolved names based
        # on a value of match distance value defined by the user.
        suggested_search <-
          bdc::bdc_suggest_names_taxadb(
            sci_name = found_name$input[not_found_index],
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

        ### Table containing original and suggested names
        original_suggested_names <- suggested_search[suggested, 1:2]

        # Searching for accepted names for each suggested names using 'taxadb'
        if (any(suggested == TRUE)) {

          # Exclude names without suggestion
          suggested_names_filtered <- suggested_name[suggested]

          # Only unique names will be returned. Two or more misspelled names can
          # have a same suggested names.
          suggested_names_filtered <- unique(suggested_names_filtered)

          # Searching for matching names for suggested names using taxadb
          suggest_data <-
            suppressWarnings(
              taxadb::filter_name(suggested_names_filtered,
                                  provider = db
              ))

          # Add three new columns (notes, original_search, and distance)
          suggest_data <-
            suggest_data %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              notes = character(nrow(suggest_data)),
              original_search = character(nrow(suggest_data)),
              distance = numeric(nrow(suggest_data))
            )

          # Create a warning when one suggested name match multiple original
          # names.
          if (any(duplicated(suggested_names_filtered))) {
            duplicated <- duplicated(suggested_names_filtered)

            report <-
              which(suggested_names_filtered %in%
                      suggested_names_filtered[duplicated])

            names_to_check <-
              suggested_search[suggested_search[, "suggested"] %in%
                                 suggested_names_filtered[report], 1:2]
            message(
              "There are more than one sci_name with the same suggested name.
               Please check whether they are not the same species with
              misspelled names:\n",
              paste0(
                "\n",
                "Original name -> Suggested name\n",
                paste(apply(names_to_check, 1, function(i) {
                  paste(i, collapse = " -> ")
                }), collapse = "\n")
              )
            )
          }

          # Filter duplicated names returned by filter_name (excluding synonyms
          # when there are valid names)
          if (any(duplicated(suggest_data$input))) {
            suggest_data <- bdc_clean_duplicates(data = suggest_data)
          }

          # Reordering the database
          suggest_data <- suggest_data[order(suggest_data$sort), ]

          ### Join original names to suggested names. Remember: the column input
          # contain the suggest names.
          suggest_data <-
            dplyr::full_join(suggest_data, original_suggested_names,
                             by = c("input" = "suggested")) %>%
            dplyr::mutate(original_search = original) %>%
            dplyr::select(-original) %>%
            dplyr::distinct(original_search, .keep_all = T)

          ### TO BE REMOVED

          # w <- which(!is.na(suggested_search$suggested) & !duplicated(suggested_search$suggested))

          # suggest_data <-
          #   suggest_data %>%
          #   dplyr::mutate(original_search = suggested_search$original[w])

          # FIXME: is it necessary? Removes duplicated names from input
          # suggest_data <-
          #   suggest_data %>% dplyr::distinct(input, .keep_all = T)

          # Adding suggested data on found_name
          posi_misspelled_names <-
            match(suggest_data$original_search, found_name$original_search)

          found_name[posi_misspelled_names, 1:ncol_tab_taxadb] <-
            suggest_data[, 1:ncol_tab_taxadb]

          # Flag misspelled names
          found_name[posi_misspelled_names, "notes"] <-
            "| wasMisspelled "

        } else {
          suggested_search <-
            data.frame(
              original = found_name$original_search,
              suggested = NA,
              distance = NA
            )
        }
      }

      # searching for accepted names to synonyms.
      synonym_index <- which(found_name$taxonomicStatus == "synonym")
      nrow_synonym <- length(synonym_index)

      if (nrow_synonym > 0L) {
        if (replace_synonyms) {
          accepted <-
            suppressWarnings(taxadb::filter_id
                             (found_name$acceptedNameUsageID[synonym_index], db))

          # Add original names
          ori_names <-
            found_name %>%
            dplyr::select(original_search) %>%
            dplyr::slice(synonym_index)

          accepted <- dplyr::bind_cols(accepted, ori_names)

          # Split names by equal original_search
          accepted_list <- split(accepted, as.factor(accepted$original_search))
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
              paste(found_name$notes[p0], "replaceSynonym", sep = "| ")
          }

          # Flag synonyms with no accepted names
          if (any(accepted_empty == TRUE)) {
            p <- match(
              names(accepted_list[accepted_empty]), found_name$original_search
            )

            found_name[p, "notes"] <-
              paste(found_name[p, "notes"][[1]], "noAcceptedName", sep = "| ")
          }

          # Flag synonyms with multiple accepted name
          if (any(nrow_accepted > 1L)) {
            p1 <- match(
              names(accepted_list[nrow_accepted > 1]),
              found_name$original_search
            )

            found_name[p1, "notes"] <-
              paste(found_name[p1, "notes"][[1]], "multipleAccepted", sep = "| ")
          }
        } # end of replace_synonyms
      } # end of nrow_synonym
    } # end of not_found

    # Export a table containing names linked to multiple accepted names.
    if (export_accepted == TRUE) {
      dir <- here::here("Output", "Check")
      fs::dir_create(dir)

      multi_accName <-
        found_name %>%
        dplyr::filter(stringr::str_detect(notes, regex("multiple"))) %>%
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
      dplyr::rename(suggested_name = suggested,
                    original_search = original_search) %>%
      dplyr::select(-c(input, sort)) %>%
      dplyr::select(original_search,
                    suggested_name,
                    distance,
                    notes,
                    everything()) %>%
      dplyr::mutate(distance = ifelse(distance > suggestion_distance,
                                      distance, NA)) %>%
      dplyr::mutate(notes = ifelse(
        is.na(scientificName) & notes != "multipleAccepted",
        'notFound',
        notes
      ))

    # Adding information on 'taxonomicStatus' in the column 'notes'.
    found_name <-
      found_name %>%
      dplyr::mutate(notes =
                      ifelse(
                        notes != "notFound" & notes != "multipleAccepted",
                        paste(found_name$taxonomicStatus, notes, sep = " "),
                        notes
                      ))

    # Returning as NA names whose 'taxonomicStatus' are different of "accepted"
    # and "synonym"
    w <-
      which(found_name$taxonomicStatus != "accepted" &
              found_name$taxonomicStatus != "synonym")

    if(length(w) > 0){
      found_name[w, 5:(ncol_tab_taxadb+2)] <- NA
    }

    # Trimming extra-spaces from the column "notes"
    found_name$notes <- stringr::str_squish(found_name$notes)


    # joining  names queried to the original (complete) database
    found_name <-
      dplyr::left_join(raw_sci_name, found_name, by = "original_search")

    end <- Sys.time()
    total_time <- round(as.numeric (end - start, units = "mins"), 1)

    message(paste(
      "\n",
      nrow(found_name),
      "names queried in",
      total_time,
      "minutes\n"
    ))

    return(found_name)
  }
