#' Internal function. Finds a potential matching candidate for misspelled names
#'
#' This algorithm searches for a potential match candidate within an orthographic distance of each misspelled names. Only names within an orthographic distance specified in 'max_distance' will be returned. This function was built based on the 'suggest.name' function of the flora package.
#'
#' @param sci_name character string. Scientific name to match a taxonomic database available in the 'taxadb' package.
#' @param max_distance numeric. Value between 0 and 1 specifying the maximum distance between the scientific names and names suggested by a fuzzy matching. Values close to 1 indicate that only a few differences between scientific names and name suggested are allowed. Default = 0.9.
#' @param provider character string. A database where the valid will be searched. The options are those provided by the 'taxadb' package.
#' @param rank_name character string. A taxonomic rank to filter the database. Options available are: "kingdom", "phylum", "class", "order", "family", and "genus". Default = NULL.
#' @param rank character string. Taxonomic rank name (e.g. "Plantae", "Animalia", "Aves", "Carnivora". Default is NULL.
#' @param parallel logical. If TRUE (Default) to run in parallel. 
#' @param ncores numeric. Number of cores to run in parallel. Default = 2.
#' @details 
#' This function looks for scientific names in the taxonomic database. First, it filters names by taxonomic rank ('rank_name' and 'rank'), then returns all names with the same first letters than original names ('sci_name'). Then, the function calculates string distances among original names and the candidate names. String distance is calculated by optimal string alignment (restricted Damerau-Levenshtein distance) that counts the number of deletions, insertions, substitutions, and transpositions of adjacent characters. It ranges from 0 to 1, being 1 an indicative of a perfect match. 
#' If a candidate name is found and the string distance is equal or higher than the distance informed in ‘max_distance’, a suggested name is returned. Otherwise, names are be returned as NA. To reduce the number of both false positives and negatives, the ‘suggest_distance’ is 0.9, by default, which means that only few differences between strings are allowed. If there are multiple candidate names with the same string distance, the first name is returned.
#' 
#' @return This function returns a data.frame whose first column is the original name, the second column is the suggested name and the third column is the distance between the sci_name and the suggested name. It is worth to note that if there are two names with equal distances, only the first one is returned.
#' @noRd
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ("Cebus apela", "Puma concolar")
#' bdc_suggest_names_taxadb(x, provider = "gbif")
#' }
bdc_suggest_names_taxadb <-
  function(sci_name,
           max_distance = suggestion_distance,
           provider = db,
           rank_name = NULL,
           rank = NULL,
           parallel = TRUE,
           ncores = 2) {
    
    # Get first letter of all scientific names
    first_letter <-
      unique(sapply(sci_name, function(i) {
        strsplit(i, "")[[1]][1]
      },
      USE.NAMES = FALSE
      ))
    
    first_letter <- base::toupper(first_letter)
    
    # Should taxonomic database be filter according to a taxonomic rank name?
    if (!is.null(rank_name) & !is.null(rank)) {
      species_first_letter <-
        taxadb::taxa_tbl(provider) %>%
        dplyr::filter(., .data[[rank]] == rank_name | is.na(.data[[rank]])) %>%
        dplyr::pull(scientificName) %>%
        grep(paste0("^", first_letter, collapse = "|"), ., value = TRUE)
    } else if (is.null(rank_name) & !is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else if (!is.null(rank_name) & is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else {
      species_first_letter <-
        taxadb::taxa_tbl(provider) %>%
        dplyr::pull(scientificName) %>%
        grep(paste0("^", first_letter, collapse = "|"), ., value = TRUE)
    }
    
    # Should parallel processing be used?
    if (parallel == TRUE) {
      # setup parallel backend to use many processors
      cl <- parallel::makeCluster(ncores) # not to overload your computer
      doParallel::registerDoParallel(cl)
      
      sug_dat <-
        foreach(i = sci_name,
                .combine = rbind, .export = "bdc_return_names") %dopar% {
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