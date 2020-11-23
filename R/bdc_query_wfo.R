
#' Title: Query names in WorldFlora Online database
#'
#' @param species_name 
#' @param match_dist 
#'
#' @return
#' @export
#'
#' @examples
bdc_query_wfo <- function(species_name, match_dist) {
  
  # Download WFO database
  wfo_data <- here::here("data", "WFO_Backbone", "classification.txt")
  wfo_dir <- here::here("data", "WFO_Backbone")
  
  if (!fs::file_exists(wfo_data)) {
    fs::dir_create(wfo_dir)
    
    WorldFlora::WFO.download(
      WFO.url = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip",
      save.dir = wfo_dir,
      WFO.remember = T
    )
    
    utils::unzip("data/WFO_Backbone/WFO_Backbone.zip", exdir = wfo_dir)
  }
  
  query_names <-
    WorldFlora::WFO.match(species_name,
                          WFO.file = "data/WFO_Backbone/classification.txt",
                          Fuzzy.min = T,
                          squish = F,
                          spec.name.nonumber = F,
                          spec.name.nobrackets = F,
                          spec.name.sub = F,
                          verbose = F,
                          counter = T
    )
  
  query_names_filter <-
    query_names %>%
    dplyr::select(Fuzzy.dist, taxonRank, scientificName, spec.name) %>%
    mutate(query_match_dist = if_else(Fuzzy.dist <= match_dist |
                                        is.na(Fuzzy.dist), T, F)) %>%
    dplyr::mutate(scientificName = ifelse(query_match_dist == T,
                                          scientificName,
                                          NA
    ))
  return(query_names_filter)
}

