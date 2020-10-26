ipak(
  c(
    "taxadb",
    "tidyverse",
    "vroom",
    "here",
    "taxize"
  )
)

# Load the database
merged_database <-
  here::here("data", "temp", "standard_database.xz") %>%
  vroom::vroom(
      file = .,
      guess_max = 10^6,
      col_types = readr::cols(
        database_id                      = readr::col_character(),
        occurrence_id                    = readr::col_double(),
        scientific_name                  = readr::col_character(),
        decimal_latitude                 = readr::col_double(),
        decimal_longitude                = readr::col_double(),
        event_date                       = readr::col_date(),
        family                           = readr::col_character(),
        country                          = readr::col_character(),
        state_province                   = readr::col_character(),
        county                           = readr::col_character(),
        coordinate_precision             = readr::col_character(),
        taxon_rank                       = readr::col_character(),
        identified_by                    = readr::col_character(),
        coordinate_uncertainty_in_meters = readr::col_character(),
        recorded_by                      = readr::col_character()
      )
    )

# Test sample
set.seed(1234)
samp_df <- dplyr::slice_sample(.data = merged_database,  n = 50)

sci_names <- samp_df$scientific_name
taxo_authority <- "gbif"



stand_taxonomy <- function(sci_names, 
                           taxo_authority = "gbif"){ 
  
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
  
  
  
  # if taxo_authority = blablalbla 
  # if taxo_authority == flora...
 
  # This one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user 
  taxadb::td_create(taxo_authority, schema = "dwc")
  
  
  # Table containing names and a temporary ID for unique names (e.g. a same name will has the same id)
  df <-
    sci_names %>%
    as_tibble() %>%
    dplyr::rename(input = value) %>% 
    dplyr::group_by(input) %>%
    dplyr::mutate(temp_id = cur_group_id()) %>%
    dplyr::ungroup()
  
    
  # Get names ids or each (unique) names
  start_time <- Sys.time()
  query_one <- 
    df %>% 
    dplyr::distinct(input) %>% 
    dplyr::pull(input) %>% 
    taxadb::filter_name(., provider = taxo_authority) %>% 
    dplyr::filter(taxonomicStatus == "accepted") %>% 
    dplyr::select(scientificName, scientificNameAuthorship, family, 
                  taxonRank, taxonomicStatus, input)
    end_time <- Sys.time()
  print(end_time - start_time)
  
  
  df <- dplyr::left_join(df, query_one, by = "input")
    
  
  # Clean names
  clean_names <- 
    df %>% 
    dplyr::filter(is.na(scientificName)) %>% 
    dplyr::select(input, temp_id) %>% 
    mutate(names_cleaned = taxadb::clean_names(input, binomial_only = T)) %>% 
    dplyr::select(-input)
  
  
  query_two <- 
    clean_names %>% 
    dplyr::pull(names_cleaned) %>% 
    taxadb::filter_name(., provider = taxo_authority) %>% 
    # dplyr::filter(taxonomicStatus == "accepted") %>% 
    dplyr::select(scientificName, scientificNameAuthorship, family, 
                  taxonRank, taxonomicStatus, input) %>% 
    dplyr::rename(names_cleaned = input)
  
  
  query_two <- left_join(query_two, clean_names, by = "names_cleaned")
  
  
  # Merge data
  w <- which(df$temp_id %in% query_two$temp_id)
  
  df$scientificName[w] <- query_two$scientificName
  df$scientificNameAuthorship[w] <- query_two$scientificNameAuthorship
  df$family[w] <- query_two$family
  df$taxonRank[w] <- query_two$taxonRank
  df$taxonomicStatus[w] <- query_two$taxonomicStatus
  
  df <- left_join(df, clean_names, by = "temp_id")
  

  # Query unresolved names in taxize
  query_three <- 
    df %>% 
    dplyr::filter(is.na(scientificName)) %>% 
    dplyr::pull(names_cleaned) %>% 
    taxize::iplant_resolve(., retrieve = "best", http = "post") %>% 
    dplyr::filter(scientificscore > 0.9) %>% 
    dplyr::select(acceptedname) %>% 
    dplyr::rename(scientificName = acceptedname) %>% 
    as_tibble()
  
  

  taxadb::filter_name("Pourouma guianensis", provider = taxo_authority)
  
  
 
 # Get names
  teste2 <- 
    teste %>% 
    filter(!is.na(taxoid)) %>% 
    # arrange(desc(sci_names)) %>% 
    dplyr::mutate(verified_name = taxadb::get_names(taxoid, db = taxo_authority))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  devtools::install_github("cboettig/taxalight")
  
  # install.packages("digest")
  # library(digest)
  
  library(taxalight)
  tl_create("gbif")
  
  
  
  
  