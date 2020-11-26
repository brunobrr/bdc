min_get_names <- function (id, db = getOption("taxadb_default_provider", "itis"), 
          version = taxadb:::latest_version(), format = c("guess", "prefix", 
                                                 "bare", "uri"), taxadb_db = taxadb::td_connect()) 
{
  format <- match.arg(format)
  n <- length(id)
  prefix_ids <- switch(format, prefix = id, taxadb:::as_prefix(id, db))
  df <- filter_id(prefix_ids, provider = db, version = version, 
                  collect = FALSE, db = taxadb_db) %>% dplyr::select("scientificName", 
                                                                     "taxonID", "sort", "taxonomicStatus") %>% dplyr::distinct() %>% min_take_first_duplicate() %>% 
    dplyr::collect()
  if (dim(df)[1] != n) {
    stop(paste("Error in resolving possible duplicate names.", 
               "Try the ids() function instead."), call. = FALSE)
  }
  df[["scientificName"]]
}

max_get_names <- function (id, db = getOption("taxadb_default_provider", "itis"), 
                           version = taxadb:::latest_version(), format = c("guess", "prefix", 
                                                                           "bare", "uri"), taxadb_db = taxadb::td_connect()) 
{
  format <- match.arg(format)
  n <- length(id)
  prefix_ids <- switch(format, prefix = id, taxadb:::as_prefix(id, db))
  df <- filter_id(prefix_ids, provider = db, version = version, 
                  collect = FALSE, db = taxadb_db) %>% dplyr::select("scientificName", 
                                                                     "taxonID", "sort", "taxonomicStatus") %>% dplyr::distinct() %>% max_take_first_duplicate() %>% 
    dplyr::collect()
  if (dim(df)[1] != n) {
    stop(paste("Error in resolving possible duplicate names.", 
               "Try the ids() function instead."), call. = FALSE)
  }
  df[["scientificName"]]
}

min_take_first_duplicate <- function (df) 
{
  scientificName <- "scientificName"
  sort <- "sort"
  row_num <- "row_num"
  n <- "n"
  max_repeated <- df %>% dplyr::count(sort, sort = T) %>% utils::head(1) %>% 
    dplyr::pull(n)
  if (max_repeated == 1) 
    return(df)
  df %>% dplyr::arrange(scientificName) %>% dplyr::mutate(row_num = dplyr::row_number()) %>% 
    dplyr::group_by(sort) %>% 
    dplyr::filter(taxonomicStatus == "accepted") %>%
    dplyr::ungroup() %>% dplyr::arrange(sort)
}


max_take_first_duplicate <- function (df) 
{
  scientificName <- "scientificName"
  sort <- "sort"
  row_num <- "row_num"
  n <- "n"
  max_repeated <- df %>% dplyr::count(sort, sort = T) %>% utils::head(1) %>% 
    dplyr::pull(n)
  if (max_repeated == 1) 
    return(df)
  df %>% dplyr::arrange(scientificName) %>% dplyr::mutate(row_num = dplyr::row_number()) %>% 
    dplyr::group_by(sort) %>% 
    dplyr::filter(taxonomicStatus == "accepted") %>%
    dplyr::ungroup() %>% dplyr::arrange(sort)
}
