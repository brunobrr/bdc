#' Matching scientific names against local stored taxonomic authority database
#'
#' This function borrows heavily from the great work done by Norman et al.
#' (2020) in their package 'taxadb', which contains functions that allow
#' querying millions of taxonomic names in a fast and consistent way. The
#' bdc_get_taxadb contains additions that allow queries using fuzzy matching and
#' converting synonyms to the current accepted name. The fuzzy match algorithm
#' was inspired by get.taxa function of the 'flora' package.
#'
#'
#' @param sci_name A character vector of species names. The function does not
#' clean species names (e.g.: infraespecific, var., inf.), it is expected
#' clean names. The inclusion of 'var.' increases name distances and it is
#' advised to set smaller suggestion_distance values.
#' @param replace_synonyms A logical value (default = TRUE) whether synonyms
#' must be replaced by the valid names found in taxadb database.
#' @param suggest_names A logical value (default = TRUE) whether species names
#' must be suggested if it is not found in the first search.
#' @param suggestion_distance A numeric value (default = 0.9). It is the
#' accepted distance between the searched names and suggested ones. Distances
#' higher than specified here are not suggested and NA is returned.
#' @param db Character string. Contains the name of the taxonomic database where
#' information should be searched. By default, 'gbif' will be installed. Use
#' 'all' to install all available taxonomic database automatically.
#' @param rank_name character string. A taxonomic rank used to be used to filter
#' data. Options available are: "kingdom", "phylum", "class", "order",
#' "family", and "genus".
#' @param rank character string. Taxonomic rank name (e.g. "Plantae",
#' "Animalia", "Aves", "Carnivora". Default is NULL.
#' @param parallel A logical value indicating whether distance calculation
#' should be done in parallel.
#' @param ncores Number of cores to run in parallel.
#' @param export_accepted A logical value (default = FALSE) whether a table is
#' exported containing all species with more than one valid name to be further
#' explored by the user.
#'
#' @return This function returns a data.frame with the same number of rows and
#' order than sci_name with the information provided by the database.
#'
#' @importFrom fs dir_create
#' @importFrom here here
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect str_squish
#' @importFrom taxadb td_create filter_name filter_id
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' sci_name <- c(
#'   "Polystachya estrellensis",
#'   "Tachigali rubiginosa",
#'   "Oxalis rhombeo ovata",
#'   "Axonopus canescens",
#'   "Prosopis",
#'   "Guapira opposita",
#'   "Clidemia naevula",
#'   "Poincianella pyramidalis",
#'   "Hymenophyllum polyanthos")
#'
#' names_harmonization <-
#'   bdc_query_names_taxadb(
#'   sci_names,
#'   replace_synonyms = TRUE,
#'   suggest_names = TRUE,
#'   suggestion_distance = 0.9,
#'   db = "gbif",
#'   parallel = FALSE,
#'   ncores = 2,
#'   export_accepted = FALSE)
#' }
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

    # Measuring execution time
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

    # Querying names using 'taxadb' (only exact match allowed)
    found_name <- suppressWarnings(taxadb::filter_name(sci_name, provider = db))

    # Creates a vector containing the number of columns of the taxonomic
    # database. This is important because the number of columns can vary in
    # different databases according to the taxonomic authority selected
    ncol_tab_taxadb <- ncol(found_name)

    # Adds three new columns (notes, original_search, and distance)
    found_name <-
      found_name %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        notes = character(nrow(found_name)),
        original_search = character(nrow(found_name)),
        distance = numeric(nrow(found_name))
      ) %>%
      dplyr::mutate(original_search = input)

    # Flags names with names with multiple accepted names
    if (nrow(found_name) != length(sci_name)) {
      found_name <-
        bdc_clean_duplicates(
          data = found_name,
          rank = rank,
          rank_name = rank_name
        )
    }

    # Reordering the database
    found_name <- found_name[order(found_name$sort), ]

    # Selects unresolved names
    not_found <-
      is.na(found_name$scientificName) &
      !grepl("multipleAccepted", found_name$notes)

    if (any(not_found == TRUE)) {
      if (suggest_names == TRUE) {
        not_found_index <- which(not_found == TRUE)

        # Searches for approximate best matches for each unresolved names based
        # on a maximum matching distance
        suggested_search <-
          bdc_suggest_names_taxadb(
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

        # Searches matching names for each suggested names using 'taxadb'
        if (any(suggested == TRUE)) {

          # Excludes names without suggestion
          suggested_names_filtered <- suggested_name[suggested]

          # Searching for matching names for suggested names
          suggest_data <-
            suppressWarnings(taxadb::filter_name(suggested_names_filtered,
              provider = db
            ))

          # Adds three new columns (notes, original_search, and distance)
          suggest_data <-
            suggest_data %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              notes = character(nrow(suggest_data)),
              original_search = character(nrow(suggest_data)),
              distance = numeric(nrow(suggest_data))
            )

          # Creates a warning when different input names matched to a same
          # suggested name
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
               Please check if they are not the same species with
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

          # Adds original_search to suggest_data
          w <- which(!is.na(suggested_search$suggested))

          suggest_data <-
            suggest_data %>%
            dplyr::mutate(original_search = suggested_search$original[w])

          # FIXME: is it necessary? Removes duplicated names from input
          suggest_data <-
            suggest_data %>% dplyr::distinct(input, .keep_all = T)

          # Adding suggested data on found_name
          posi_misspelled_names <-
            which(found_name$original_search %in% suggest_data$original_search)

          found_name[posi_misspelled_names, 1:ncol_tab_taxadb] <-
            suggest_data[, 1:ncol_tab_taxadb]

          # Indicating misspelled names
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

      # searching accepted names for synonyms
      synonym_index <- which(found_name$taxonomicStatus == "synonym")
      nrow_synonym <- length(synonym_index)

      if (nrow_synonym > 0L) {
        if (replace_synonyms) {
          accepted <-
            suppressWarnings(taxadb::filter_id
            (found_name$acceptedNameUsageID[synonym_index], db))

          # Adds original (input) names to 'accepted'
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

          # Flags synonyms without accepted names
          if (any(accepted_empty == TRUE)) {
            p <- match(
              names(accepted_list[accepted_empty]), found_name$original_search
            )

            found_name[p, "notes"] <-
              paste(found_name[p, "notes"][[1]], "noAcceptedName", sep = "| ")
          }

          # Flags synonyms with +1 accepted name
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

    # Export a table with all accepted names which link to the same accepted id
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

    # Adds 'taxonomicStatus' to the column 'notes'
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

    found_name[w, 5:(ncol_tab_taxadb+2)] <- NA

    # Trimming extra-spaces from the column "notes
    found_name$notes <- stringr::str_squish(found_name$notes)

    # joining  names queried to the original (complete) list of names
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

