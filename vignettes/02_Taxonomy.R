# Load all functions of bdc workflow
devtools::load_all()

# Install and load packages
ipak(
  c(
    "taxadb",
    "tidyverse",
    "vroom",
    "here",
    "rgnparser", 
    "flora"
  )
)

# FIXME: Check whether all directories are needed

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))
fs::dir_create(here::here("Output/Report"))
fs::dir_create(here::here("Output/Figures"))

# Load data ---------------------------------------------------------------
# Load the database resulting from the prefilter step
prefilter_database <-
  here::here("Output", "Intermediate", "01_database.qs") %>%
  qs::qread()

sci_names <-
  prefilter_database %>%
  dplyr::pull(scientificName)

taxo_authority <- "gbif"
wfo_match_dist = 6
replace_synonyms = T

# Select one taxonomic authority. Options currently recognized by taxadb are:
# - itis: Integrated Taxonomic Information System
# - ncbi: National Center for Biotechnology Information
# - col: Catalogue of Life
# - tpl: The Plant List
# - gbif: Global Biodiversity Information Facility
# - fb: FishBase
# - slb: SeaLifeBase
# - wd: Wikidata
# - ott: OpenTree Taxonomy
# - iucn: IUCN Red List


# Function for standardize names
standardize_taxonomy <- function(sci_names,
                                 taxo_authority = "gbif",
                                 wfo_match_dist = 6,
                                 replace_synonyms = TRUE) {
  

  # this one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user
  taxadb::td_create(taxo_authority, schema = "dwc", overwrite = FALSE)
  
  # one-time setup to download and install rgnparser
  rgnparser::install_gnparser(force = F)
  
  # create a temporary ID for unique names (e.g. a same name will has the same id)
  df0 <-
    sci_names %>%
    as_tibble() %>%
    dplyr::rename(input = value) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(temp_id = cur_group_id()) %>%
    dplyr::ungroup() 
  
  # select only unique names
  df <- 
    df0 %>% 
    distinct(temp_id, .keep_all = T) %>% 
    dplyr::mutate_all(na_if,"") %>% 
    dplyr::filter(!is.na(input))
  
  
  # routines to clean and parse names (see script "aux_function" for checking functions used to clean and parse names)
  
  parse_names <- df
  
  # remove family names 
  rem_family <-
    parse_names %>% 
    pull(input) %>%
    rem_family_names()
  
  parse_names <- 
    parse_names %>% 
    mutate(clean_family_names = rem_family$sp_names) %>% 
    mutate(flag_family_names = rem_family$fag_family)
  
  # Remove taxonomic uncertainty terms
  rem_uncer <- 
    parse_names %>% 
    pull(clean_family_names) %>%
    rem_taxo_unc()
  
  parse_names <- 
    parse_names %>% 
    mutate(clean_uncer_terms = rem_uncer)
  
  # Flag taxonomic uncertainty terms
  flag_uncer<- 
    parse_names %>% 
    pull(clean_family_names) %>%
    flag_taxo_unc()
  
  parse_names <- 
    parse_names %>% 
    mutate(uncer_terms = flag_uncer$term_uncertainty) %>% 
    mutate(flag_uncer_terms = flag_uncer$taxo_uncertainty)
  
  # Remove duplicated generic names, extra spaces, capitalize the generic name
  other_issues <-
    parse_names %>% 
    pull(clean_uncer_terms) %>% 
    rem_other_issues(.)
  
  parse_names <- 
    parse_names %>% 
    mutate(clean_other_issues = other_issues) %>% 
    rename(name_clean = clean_other_issues)
  
  # Parse names using rgnparser 
  gnparser <-
    parse_names %>%
    pull(name_clean) %>%
    rgnparser::gn_parse_tidy() %>%
    select(verbatim, cardinality, canonicalfull, quality) %>% 
    rename(name_clean = verbatim)
  
  
  # Names parsed
  parse_names <-
    full_join(parse_names, gnparser, by = "name_clean") %>% 
    rename(input_cleaned = canonicalfull) %>% 
    distinct(temp_id, .keep_all = T)
  
  # FIXEME: Delete line code below
  # parse_names <- read.csv("parse_names.csv", h=T)
  parse_names_sel <- 
    parse_names %>% 
    dplyr::select(input, temp_id, input_cleaned)
  
  
  # Merge with df
  df <- full_join(df, parse_names_sel)
  
  
  # query one: look up taxonomic information in taxadb
  query_one <- 
    df %>%
    dplyr::distinct(input_cleaned, .keep_all = T) %>%
    dplyr::pull(input_cleaned) %>%
    taxadb::filter_name(., provider = taxo_authority) %>%
    dplyr::select(scientificName,
                  scientificNameAuthorship,
                  family,
                  taxonRank,
                  taxonomicStatus,
                  acceptedNameUsageID,
                  input) %>% 
    rename(input_cleaned = input)
  
  # merge data
  df <- dplyr::full_join(df, query_one, by = "input_cleaned")
  
  
  
  
  # Search for another possible names (synonyms or accepted ones) of unresolved names in World Flora online (WFO) database
  
  # Number of unresolved names
  names_NA <- 
    df %>%
    dplyr::filter(is.na(scientificName))
  
  if (nrow(names_NA) != 0){
    
    # One-time setup: download WFO database
    wfo_data <- here::here("data", "WFO_Backbone", "classification.txt")
    wfo_dir <- here::here("data", "WFO_Backbone")
    
    if (!fs::file_exists(wfo_data)) {
      fs::dir_create(wfo_dir)
      
      WorldFlora::WFO.download(WFO.url = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip",
                               save.dir = wfo_dir,
                               WFO.remember = T)
      
      utils::unzip("data/WFO_Backbone/WFO_Backbone.zip", exdir = wfo_dir)
    }
    
    query_names_wfo <-
      names_NA %>%
      dplyr::pull(input_cleaned) %>%
      query_wfo(., wfo_match_dist) %>% 
      dplyr::rename(names_suggested_wfo = scientificName)
    
    
    Wfo_not_found <- is.na(query_names_wfo$names_suggested_wfo) %>% all
    
    if (Wfo_not_found == F) {
    
    # Use names retrieved from WFO to carry out a new query for accepted names in taxadb
      query_two <-
        query_names_wfo %>%
        dplyr::filter(!is.na(names_suggested_wfo)) %>%
        dplyr::pull(names_suggested_wfo) %>%
        taxadb::filter_name(., provider = taxo_authority) %>%
        dplyr::select(
          scientificName,
          scientificNameAuthorship,
          family,
          taxonRank,
          taxonomicStatus,
          acceptedNameUsageID,
          input
        ) %>%
        dplyr::rename(names_suggested_wfo = input)
      
      
      query_two <-
        query_names_wfo %>%
        dplyr::select(names_suggested_wfo, spec.name) %>%
        dplyr::full_join(., query_two, by = "names_suggested_wfo") %>%
        dplyr::rename(input_cleaned = spec.name)
      
      
      query_two_join <-
        df %>% 
        dplyr::select(input, input_cleaned, temp_id) %>% 
        dplyr::filter(input_cleaned %in% query_two$input_cleaned) %>% 
        dplyr::full_join(query_two, ., by = "input_cleaned") %>%
        dplyr::select(-names_suggested_wfo) %>%
        dplyr::select(names(df))
      
    }
    
    # Merge data
    df_temp <- dplyr::bind_rows(df, query_two_join)
    
  }else{
    df_temp <- names_NA %>%
      dplyr::bind_rows(df, .)
  }
  
  
  # filter only accepted names
  names_resolved <-
    df_temp %>%
    dplyr::filter(taxonomicStatus == "accepted")
  
  # find names with more than one accepted names. Those names will be added to the table of unresolved names
  names_accDup <-
    names_resolved %>%
    janitor::get_dupes(temp_id) %>% 
    dplyr::select(-dupe_count) %>% 
    dplyr::select(names(df))
  
  rem_accNames <- ifelse(nrow(names_accDup) > 0, T, F)
  
  # remove names with more than one accepted name
  if (rem_accNames == T) {
    names_resolved <-
      names_resolved %>%
      dplyr::filter(!temp_id %in% names_accDup$temp_id)
  }
  
  # filter names not accepted and/or those that remain unresolved
  unresolved_names <-
    df %>%
    dplyr::filter(!(taxonomicStatus == "accepted") | is.na(taxonomicStatus))
  
  # identify synonym names already resolved and merge names with more than one accepted name
  unresolved_names <-
    unresolved_names %>%
    dplyr::filter(!temp_id %in% names_resolved$temp_id) %>% 
    filter(!temp_id %in% names_accDup$temp_id)
  
  
  
  # resolve synonyms that have only one accepted name id
  
  if (replace_synonyms == T){
    
    df_synonyms <- 
      unresolved_names %>% 
      group_by(input, scientificName, temp_id) %>% 
      summarise(unique_accepName = n_distinct(acceptedNameUsageID)) %>% 
      ungroup() %>% 
      filter(unique_accepName == 1 & !is.na(scientificName)) %>% 
      dplyr::select(-unique_accepName)
    
    # Add column acceptedNameUsageID
    df_synonyms_id <- 
      unresolved_names %>% 
      dplyr::select(temp_id, acceptedNameUsageID) %>% 
      dplyr::distinct(., .keep_all = T) %>% 
      dplyr::filter(!is.na(acceptedNameUsageID)) %>% 
      full_join(df_synonyms, unresolved_names, by = "temp_id") %>% 
      dplyr::filter(!is.na(scientificName)) %>% 
      dplyr::rename(input_cleaned = scientificName)
    
    # Use synonyms' id to query for accepted names (a vector is retrieved)
    search_accNames <- 
      df_synonyms_id %>% 
      dplyr::pull(acceptedNameUsageID) %>%
      taxadb::get_names(., db = taxo_authority)

    # Add accepted names
    df_synonyms_id <- 
      df_synonyms_id %>% 
      dplyr::mutate(retrieved_accepName = search_accNames)
    
    
    # use the name retrieved in filter_name function (taxadb)
    query_synonyms <-
      df_synonyms_id %>%
      dplyr::filter(!is.na(retrieved_accepName)) %>% 
      dplyr::pull(retrieved_accepName) %>%
      taxadb::filter_name(., provider = taxo_authority) %>%
      dplyr::select(
        scientificName, scientificNameAuthorship, family,
        taxonRank, taxonomicStatus, acceptedNameUsageID, input
      ) %>%
      dplyr::rename(retrieved_accepName = input)
    
    query_synonyms_join <-
      df_synonyms_id %>% 
      dplyr::select(temp_id, input, input_cleaned, retrieved_accepName) %>% 
      dplyr::full_join(query_synonyms, df_synonyms_id, by = "retrieved_accepName") %>%
      dplyr::select(-retrieved_accepName) %>% 
      dplyr::select(names(df))
    
    # Add names to resolved or unresolved tables
    names_resolved <- 
      query_synonyms_join %>% 
      dplyr::filter(!is.na(scientificName)) %>% 
      dplyr::bind_rows(names_resolved, .)
    
    unresolved_names <- 
      unresolved_names %>% 
      dplyr::filter(!temp_id %in% query_synonyms_join$temp_id) # replace found names
      
    unresolved_names <- 
      query_synonyms_join %>% 
      dplyr::filter(is.na(scientificName)) %>% 
      dplyr::bind_rows(unresolved_names, .)
  }
  
  # Unresolved names are those not resolved or with more than one accepted name (or synonyms if replace_synonyms == T)
  unresolved_names <- 
  dplyr::bind_rows(unresolved_names, names_accDup)

  # Replace to NA
  unresolved_names_filter <- 
    unresolved_names %>% 
    mutate(across(scientificName:acceptedNameUsageID, 
                  ~if_else(!is.na(.), NA, NA))) %>% 
    dplyr::distinct(input, .keep_all = T)
  
  df_final <-
    rbind(names_resolved, unresolved_names_filter) %>% 
    dplyr::full_join(df0, ., by = c("temp_id", "input"))
  
  df_final <- 
    parse_names %>% 
    dplyr::select(uncer_terms, flag_uncer_terms, temp_id) %>% 
    dplyr::full_join(df_final, ., by = "temp_id") %>% 
    rename(.flag_uncer_terms = flag_uncer_terms) %>% 
    dplyr::select(-temp_id)

  
  # create  directories to salve files
  save_in_dir_che <- here::here("output", "Check", "02_taxonomy")
  save_in_dir_int <- here::here("output", "Intermediate", "02_taxonomy")
  
  if (!fs::dir_exists(save_in_dir_che) & !fs::dir_exists(save_in_dir_int)) {
    fs::dir_create(save_in_dir_che)
    fs::dir_create(save_in_dir_int)
  }
  
  
  # save files
  
  df_final %>%
    vroom::vroom_write(paste0(save_in_dir_int, "/02_taxonomy.csv"))
  
  unresolved_names %>%
    vroom::vroom_write(paste0(save_in_dir_che, "/02_unresolved_names.csv"))
  
  parse_names %>%
    vroom::vroom_write(paste0(save_in_dir_che, "/02_names_parsed.csv"))
  
  return(df_final)
}