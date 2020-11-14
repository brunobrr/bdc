ipak(
  c(
    "taxadb",
    "tidyverse",
    "vroom",
    "here",
    "rgnparser"
  )
)

# Load the database
merged_database <- vroom("data/temp/standard_database.xz")

# Test sample
set.seed(1234)
samp_df <- dplyr::slice_sample(.data = merged_database, n = 300)

sci_names <-
  samp_df %>%
  dplyr::pull(scientificName)

taxo_authority <- "gbif"

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
                                 match_dist = 6) {

  # if taxo_authority = blablalbla
  # if taxo_authority == flora...

  # this one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user
  taxadb::td_create(taxo_authority, schema = "dwc", overwrite = FALSE)

  # one-time setup to download and install rgnparser
  rgnparser::install_gnparser()
  
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
    dplyr::filter(!is.na(input)) 
  
  
  df0 %>% 
    dplyr::filter(!is.na(input)) 
  
  # query one: look up taxonomic information by scientific name in taxadb using verbatim scientific name
  query_one <- df %>%
    dplyr::distinct(input) %>%
    dplyr::pull(input) %>%
    taxadb::filter_name(., provider = taxo_authority) %>%
    dplyr::select(scientificName,
                  scientificNameAuthorship,
                  family,
                  taxonRank,
                  taxonomicStatus,
                  input)
  
  # merge data
  df <- dplyr::full_join(df, query_one, by = "input")

  
  
  # routines to clean and parse names (see script "aux_function" for checking functions used to clean and parse names)
  
  parse_names <- 
    df %>%
    dplyr::filter(is.na(scientificName)) %>%
    dplyr::select(input, temp_id)
  
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
  
  parse_names <-
    left_join(parse_names, gnparser, by = "name_clean") %>% 
    rename(input_cleaned = canonicalfull) %>% 
    distinct(temp_id, .keep_all = T)
  
  parse_names_sel <- 
    parse_names %>% 
    dplyr::select(input, temp_id, input_cleaned)
  
  # query two: look up taxonomic information by scientific name in taxadb using parsed scientific name
  
  # filter names already not found
  not_found_names <- which(parse_names_sel$input == parse_names_sel$input_cleaned)
  
  if (length(not_found_names) > 0) {
    parse_names_sel <- parse_names_sel[-not_found_names,]
  }
  
  query_two <-
    parse_names_sel %>%
    dplyr::pull(input_cleaned) %>%
    taxadb::filter_name(., provider = taxo_authority) %>%
    dplyr::select(
      scientificName, scientificNameAuthorship, family,
      taxonRank, taxonomicStatus, input
    ) %>%
    dplyr::rename(input_cleaned = input)


  # Add temp_id and reorder columns
  query_two <-
    full_join(query_two, parse_names_sel, by = "input_cleaned") %>%
    dplyr::select(-input_cleaned) %>% 
    dplyr::select(names(df))

  # Merge data found at first and second search
  df <-
    df %>%
    dplyr::filter(!is.na(scientificName)) %>%
    dplyr::bind_rows(., query_two)

   # Query unresolved names using wfo database
  
  # Number of unresolved names
  names_NA <- 
    df %>%
    dplyr::filter(is.na(scientificName))
  
  w <- which(parse_names_sel$input %in% names_NA$input)
  names_NA$input_cleaned <- parse_names_sel$input_cleaned[w]
  

  if (nrow(names_NA) != 0){
    
    # Download WFO database
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
      query_wfo(., match_dist = 6) %>% 
      dplyr::rename(names_cleaned2 = scientificName)
    
    
    Wfo_not_found <- is.na(query_names_wfo$names_cleaned2) %>% all
    
    #Fixme select only names different from original names
    if (Wfo_not_found == F) {
      query_three <-
        query_names_wfo %>%
        dplyr::filter(!is.na(names_cleaned2)) %>%
        dplyr::pull(names_cleaned2) %>%
        taxadb::filter_name(., provider = taxo_authority) %>%
        dplyr::select(
          scientificName,
          scientificNameAuthorship,
          family,
          taxonRank,
          taxonomicStatus,
          input
        ) %>%
        dplyr::rename(names_cleaned2 = input)
      
      
      query_three_join <-
        query_names_wfo %>%
        dplyr::select(names_cleaned2, spec.name) %>%
        full_join(., query_three, by = "names_cleaned2") %>%
        rename(input_cleaned = spec.name)
      
      query_three_join <-
        left_join(query_three_join, parse_names_sel, by = "input_cleaned") %>%
        dplyr::select(-input_cleaned) %>%
        dplyr::select(names(df))
      
    }
    
    # Merge data
    df_temp <- dplyr::bind_rows(df, query_three_join)
    
  }else{
    df_temp <- names_NA %>%
      dplyr::select(-input_cleaned) %>%
      dplyr::bind_rows(df, .)
  }
  
  
  # Filter only accepted names
  names_resolved <-
    df_temp %>%
    dplyr::filter(taxonomicStatus == "accepted")

  # Find names with more than one accepted names whose will be added to the table containing unresolved names
    # criar aqui uma tablea com os nomes duplicados para inserir na tabela final?
  names_accDup <-
    names_resolved %>%
    janitor::get_dupes(temp_id)

    
  # Remove names with more than one accepted name
  if (nrow(names_accDup) != 0) {
      names_resolved <-
      names_resolved %>%
      dplyr::filter(!temp_id %in% names_accDup$temp_id)
  }

  # Filter names not accepted and/or not found names
  unresolved_names <-
    df %>%
    dplyr::filter(!(taxonomicStatus == "accepted") | is.na(taxonomicStatus))

  # Identify synonym names already resolved and merge names with more than one accepted name
  unresolved_names <-
    unresolved_names %>%
    dplyr::filter(!temp_id %in% names_resolved$temp_id) %>%
    dplyr::bind_rows(., names_accDup) %>%
    dplyr::select(-dupe_count)

  ## It is working!!!
  ###nrow(names_resolved)+nrow(unresolved_names)  
  
  teste<-rbind(names_resolved, unresolved_names)
  
  
  # FIXEME: Add this part at the end (out taxadb and flora functions)
  save_in_dir <- here::here("output", "02_taxonomy")

  if (!fs::dir_exists(save_in_dir)) {
    fs::dir_create(save_in_dir)
  }


  # Save files
  names_resolved %>%
    arrange(factor(input, levels = sci_names)) %>%
    vroom::vroom_write(paste0(save_in_dir, "/02_db_taxonomy.csv"))


  unresolved_names %>%
    vroom::vroom_write(paste0(save_in_dir, "/02_unresolved_names.csv"))
  
  return(names_resolved)
}










devtools::install_github("cboettig/taxalight")

# install.packages("digest")
# library(digest)

library(taxalight)
tl_create("gbif")
