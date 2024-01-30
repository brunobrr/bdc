#' Internal function. Find a potential matching candidate for misspelled names
#'
#' This function searches for a potential match candidate within an orthographic
#' distance of each misspelled names. Only names within an orthographic distance
#' specified in 'max_distance' are returned. It was built based on
#' the 'suggest.name' function of the flora package.
#'
#' @param sci_name character string. Scientific name to match a taxonomic
#' database available in the 'taxadb' package.
#' @param max_distance numeric. Value between 0 and 1 specifying the maximum
#' distance between the scientific names and names suggested by a fuzzy
#' matching. Values close to 1 indicate that only a few differences between
#' scientific names and name suggested are allowed. Default = 0.9.
#' @param provider character string. A database where the valid will be
#' searched. The options are those provided by the 'taxadb' package.
#' @param db_version numeric string. A database version (eg. 2002).
#' @param rank_name character string. Taxonomic rank name (e.g. "Plantae",
#' "Animalia", "Aves", "Carnivora". Default = NULL.
#' @param rank character string. A taxonomic rank to filter the database.
#' Options available are: "kingdom", "phylum", "class", "order", "family", and
#' "genus". Default = NULL.
#' @param parallel logical. If TRUE (Default) to run in parallel.
#' @param ncores numeric. Number of cores to run in parallel. Default = 2.
#'
#' @details This function looks for scientific names in a reference taxonomic
#' database ('db'). Higher taxonomic rank information and the first letter of
#' names supplied are used to filter the reference database.  Then, it is
#' calculated a string distance among original names and candidate names.
#' String distance is calculated by optimal string alignment (restricted
#' Damerau-Levenshtein distance) that counts the number of deletions,
#' insertions, substitutions, and transpositions of adjacent characters. It
#' ranges from 0 to 1, being 1 an indicative of a perfect match. If a
#' candidate name is found and the string distance is equal or higher than the
#' distance informed in ‘max_distance’, a suggested name is returned.
#' Otherwise, names are returned as NA. To reduce the number of both false
#' positives and negatives, the ‘suggest_distance’ is 0.9, by default, which
#' means that only a few differences between original and candidate names are
#' allowed. When multiple candidate names have an identical matching distance
#' for the original name, the name with the lowest alphabetical sort order is
#' presented as the best match.
#'
#' @return A three-column data.frame containing original name, names suggested,
#' and string distance between original and suggested (candidates) names.
#'
#' @importFrom dplyr filter pull
#' @importFrom parallel makeCluster stopCluster
#' @importFrom taxadb taxa_tbl
#' @importFrom foreach %dopar%
#' @importFrom utils head
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' x <- c("Cebus apela", "Puma concolar")
#' bdc_suggest_names_taxadb(
#'   x,
#'   max_distance = 0.75,
#'   provider = "gbif",
#'   rank_name = "Plantae",
#'   rank = "kingdom",
#'   parallel = TRUE,
#'   ncores = 2
#' )
#' }
bdc_suggest_names_taxadb <-
  function(sci_name,
           max_distance = suggestion_distance,
           provider = db,
           db_version = db_version,
           rank_name = NULL,
           rank = NULL,
           parallel = TRUE,
           ncores = 2) {
    suggestion_distance <- db <- . <- .data <- scientificName <- NULL

    # FIXME: set a env var for now
    # REVIEW: https://github.com/ropensci/taxadb/issues/91
    # Sys.setenv("CONTENTID_REGISTRIES" = "https://hash-archive.carlboettiger.info")
    # Sys.setenv("TAXADB_DRIVER"="MonetDBLite")

    # Get first letter of all scientific names
    first_letter <-
      unique(sapply(sci_name, function(i) {
        strsplit(i, "")[[1]][1]
      },
      USE.NAMES = FALSE
      ))

    name_to_case_check <- suppressMessages(taxadb::taxa_tbl(provider
                                           # , version = getOption("taxadb_default_provider", db_version)
                                           )) %>%
      dplyr::filter(!is.na(scientificName)) %>%
      utils::head(1) %>%
      pull(scientificName) %>%
      stringr::str_split(., "")



    # if (lower_case) {
    #   first_letter <- tolower(first_letter)
    # } else {
      first_letter <- toupper(first_letter)
    # }



    # Should taxonomic database be filtered according to a taxonomic rank name?
    if (!is.null(rank_name) & !is.null(rank)) {
      species_first_letter <-
        suppressMessages(taxadb::taxa_tbl(provider)) %>%
        dplyr::filter(., .data[[rank]] == rank_name | is.na(.data[[rank]])) %>%
        dplyr::pull(scientificName) %>%
        grep(paste0("^", first_letter, collapse = "|"), ., value = TRUE)
    } else if (is.null(rank_name) & !is.null(rank)) {
      stop("Please, provide both 'rank_name' and 'rank' arguments")
    } else if (!is.null(rank_name) & is.null(rank)) {
      stop("Please, provide both 'rank_name' and 'rank' arguments")
    } else {
      species_first_letter <-
        suppressMessages(taxadb::taxa_tbl(provider)) %>%
        dplyr::pull(scientificName) %>%
        grep(paste0("^", first_letter, collapse = "|"), ., value = TRUE)
    }

    # Should parallel processing be used?
    if (parallel == TRUE) {
      # setup parallel backend to use many processors
      cl <- parallel::makeCluster(ncores) # not to overload your computer
      doParallel::registerDoParallel(cl)

      sug_dat <-
        foreach::foreach(
          i = sci_name,
          .combine = rbind, .export = "bdc_return_names"
        ) %dopar% {
          bdc_return_names(i, max_distance, species_first_letter)
        } # end foreach
      parallel::stopCluster(cl) # stop cluster
    } else {
      sug_dat <-
        data.frame(
          original = character(length(sci_name)),
          suggested = character(length(sci_name)),
          distance = numeric(length(sci_name))
        )

      for (i in seq_along(sci_name)) {
        sug_dat[i, ] <-
          bdc_return_names(sci_name[i], max_distance, species_first_letter)
      }
    }
    return(sug_dat)
  }
