


bdc_filter_name <- function (x, db, version = latest_version())
{
 
  x <- tolower(x)
  
  db_tbl <- dplyr::mutate(taxadb::taxa_tbl(db, schema = "dwc", version, 
                                           taxadb::td_connect()), input = !!dplyr::sym("scientificName"))
  
  species_tab <- tibble::tibble(input = x, sort = seq_along(x))
 
  
  
  out <- dplyr::collect(dplyr::right_join(db_tbl, species_tab, by = "input", copy = TRUE))
  
  
  return(dplyr::relocate(out, sort, .before = taxonID))
  
}


bdc_filter_id <- function (x, db, version = latest_version())
{
  
  db_tbl <- dplyr::mutate(taxadb::taxa_tbl(db, schema = "dwc", version, 
                                           taxadb::td_connect()), input = !!dplyr::sym("taxonID"))
  
  species_tab <- tibble::tibble(input = x, sort = seq_along(x))
  
  
  
  out <- dplyr::collect(dplyr::right_join(db_tbl, species_tab, by = "input", copy = TRUE))
  
  
  return(dplyr::relocate(out, sort, .after = taxonID))
  
}
