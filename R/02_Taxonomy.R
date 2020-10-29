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
                           taxo_authority = "gbif", 
                           ){ 
  
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
  
  
  # Get names of each (unique) names
  start_time <- Sys.time()
  query_one <- 
    df %>% 
    dplyr::distinct(input) %>% 
    dplyr::pull(input) %>% 
    taxadb::filter_name(., provider = taxo_authority) %>% 
    # dplyr::filter(taxonomicStatus == "accepted") %>% 
    dplyr::select(scientificName, scientificNameAuthorship, family, 
                  taxonRank, taxonomicStatus, input)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  
  # Merge data
  df <- dplyr::left_join(df, query_one, by = "input") %>% 
    dplyr::mutate(names_cleaned = NA)
  
  
  # Clean names
  clean_names <- 
    df %>% 
    dplyr::filter(is.na(scientificName)) %>% 
    dplyr::select(input, temp_id) %>% 
    mutate(names_cleaned = taxadb::clean_names(input, binomial_only = T))
  
  
  query_two <- 
    clean_names %>% 
    dplyr::pull(names_cleaned) %>% 
    taxadb::filter_name(., provider = taxo_authority) %>% 
    # dplyr::filter(taxonomicStatus == "accepted") %>%
    dplyr::select(scientificName, scientificNameAuthorship, family, 
                  taxonRank, taxonomicStatus, input) %>% 
    dplyr::rename(names_cleaned = input)
  
  
  # Add temp_id and reorder columns
  query_two <-
    left_join(query_two, clean_names, by = "names_cleaned") %>%
    dplyr::select(names(df))
  
  
  # Merge data
  df <- df %>% 
    dplyr::filter(!is.na(scientificName)) %>%
    dplyr::bind_rows(., query_two)
  
  
  # Query unresolved names in taxize
  query_taxize <- 
    df %>% 
    dplyr::filter(is.na(scientificName)) %>% 
    dplyr::pull(names_cleaned) %>%
    taxize::iplant_resolve(., retrieve = "best", http = "post") %>% 
    dplyr::select(namesubmitted, acceptedname, scientificscore) %>% 
    dplyr::rename(names_cleaned = namesubmitted, names_cleaned2 = acceptedname)
  
  
  # Add temp_id
  query_taxize <-
    left_join(query_taxize, clean_names, by = "names_cleaned") 
  
  
  # FIXEme filter names according to the score (eg > 0.9)
  
  
  # Using names retrieved from taxize to search to resolve names by using taxadb
  query_three <- 
    query_taxize %>% 
    dplyr::pull(names_cleaned2) %>% 
    taxadb::filter_name(., provider = taxo_authority) %>% 
    dplyr::select(scientificName, scientificNameAuthorship, family, 
                  taxonRank, taxonomicStatus, input) %>% 
    dplyr::rename(names_cleaned2 = input)
  
  
  query_three <-
    left_join(query_three, query_taxize, by = "names_cleaned2") %>%
    dplyr::select(-c(names_cleaned, scientificscore)) %>% 
    dplyr::rename(names_cleaned = names_cleaned2) %>% 
    dplyr::select(names(df))
  
  
  # Merge data
  df <- df %>% 
    dplyr::filter(!is.na(scientificName)) %>%
    dplyr::bind_rows(., query_three)
  

  # Filter only accepted names
  df_accep <- 
    df %>%
    dplyr::filter(taxonomicStatus == "accepted")
  
  
  # Find names with more than one accepted names whose will be added to the table containing unresolved names
  dup_accep <- 
    df_accep %>% 
    janitor::get_dupes(temp_id)
  
  
  # Remove names with more than one accepted name
  df_accep <- 
    df_accep %>% 
    dplyr::filter(!temp_id %in% dup_accep$temp_id)

  
    # Filter names not accepted and/or not found names
  not_resolved <-
    df %>%
    dplyr::filter(!(taxonomicStatus == "accepted") | is.na(taxonomicStatus))
  
  
  # Identify synonym names already resolved and merge names with more than one accepted name 
  not_resolved <- 
    not_resolved %>% 
    dplyr::filter(!temp_id %in% df_acep$temp_id) %>% 
    dplyr::bind_rows(., dup_accep)
  
  # Garantir que a tabela de resultados está na mesma order do vetor inicial
  # Salvar as tabela de resultados (nome aceitos e não encontrados)
  # Fechar a função
  # Criar o diretório onde as tabelas serão salvas
  
  
  
  
  # # Get unique accepted names
  # df_accep_final <-
  #   df_accep[!(df_accep$temp_id %in% dup_accep$temp_id),]
  # 
  # # Filter not accepted and/or not found names
  # not_resolved<- 
  #   df %>%
  #   dplyr::filter(!(taxonomicStatus=="accepted")) 
  # 
  # # Identify  synonym names already resolved: accepted name founded  
  # not_resolved<- not_resolved[-c(which(not_resolved$temp_id %in% df_acep$temp_id)), ] 
  # 
  # # Add to unsolved names, those names with more than one accepted name s
  # not_resolved_final <-
  #   bind_rows(not_resolved, df_accep[(df_accep$temp_id %in% dup_accep$temp_id),])
  # 
  # # Remove duplicated names
  # not_resolved_final <-
  #   not_resolved_final[!duplicated(not_resolved_final$temp_id),] 
  # 
  # 
  
  
  
  
  
  
  
  devtools::install_github("cboettig/taxalight")
  
  # install.packages("digest")
  # library(digest)
  
  library(taxalight)
  tl_create("gbif")
  
  
  
  
  