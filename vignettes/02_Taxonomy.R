# Load all functions of bdc workflow --------------------------------------
devtools::load_all()

# Install and load packages required
ipak(
  c(
    "taxadb",
    "tidyverse",
    "vroom",
    "here",
    "rgnparser", 
    "stringr",
    "flora", 
    "parallel", 
    "doParallel", 
    "foreach"
  )
)


# Create directories for saving the results. If not existing, four new folders will be created in the folder 'Output'
bdc_create_dir()

# Load database -----------------------------------------------------------
# Load the database resulting from the prefilter step or your own database
database <-
  here::here("Output", "Intermediate", "01_prefilter_database.qs") %>%
  qs::qread()

# Standardize characters encoding
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}


# Parse scientific names --------------------------------------------------

# routines to clean and parse names (see the help of each function starting with "bdc" for more details)

# Summary of each test:

# bdc_rem_family_names: Remove family names from scientific names (e.g. Felidae Panthera onca to Panthera onca; Lauraceae Ocotea odorifera to Ocotea odorifera)

# bdc_rem_taxo_unc: Flag, identify, and remove taxonomic uncertainty terms (e.g. Myrcia cf. splendens to Myrcia splendens). Check ?bdc_bdc_rem_taxo_unc for a list of terms denoting taxonomic uncertainty and their orthographic variations.

# bdc_rem_other_issues: Convert to lower case and capitalize the only first letter of the generic names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae) and remove extra spaces

# bdc_rem_infaesp_names: Flag, identify, and remove infraespecific terms (subspecies, variety and forma)

# bdc_gnparser: Extract just binomial scientific names (without year or authors). To do this, a scientific name is breaks down in different components using rgnparser package.

# Select unique names
uniq_sciNames <- 
  database %>% 
  dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
  dplyr::select(scientificName) %>% # select this column
  dplyr::mutate_all(na_if,"") %>% # change empty names to NA
  dplyr::filter(!is.na(scientificName)) # remove NAs

# Parse names
parse_names <- 
  bdc_rem_family_names(data = uniq_sciNames, sci_names = "scientificName") %>% 
  bdc_rem_taxo_unc(data = ., sci_names = "clean_family_names") %>% 
  bdc_rem_other_issues(data = ., sci_names = "clean_uncer_terms")  %>% 
  bdc_rem_infaesp_names(data = ., sci_names = "clean_other_issues") %>% 
  bdc_gnparser(data = ., sci_names = "clean_infaesp_names")

# Save a database containing names parsed
temp <- database %>% dplyr::select(scientificName)
parse_names %>%
  dplyr::full_join(temp, ., by = "scientificName") %>% 
  data.table::fwrite(., here::here("Output", "Check", "02_parsed_names.csv"))

# Merge unique names parsed to full database and save the results of the parsing names process. Note that only the column "names_parsed" will be used in the downstream analyses. The results of each step of the parsing names process can be checked in "Output/Check/02_parsed_names.qs"
database <- 
  parse_names %>%
  dplyr::select(scientificName, .uncer_terms, .infraesp_names, names_parsed) %>% 
  dplyr::full_join(database, ., by = "scientificName")

# FIXME: delete this file 
database <- qs::qread("database_exe_.qs")
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}


# Standardize taxonomic names ---------------------------------------------

# This is made in three steps. First, names are queried using a main taxonomic authority. Next, synonyms or accepted names of unresolved names are queried using a second taxonomic authority. Finally, scientific names found in step two are used to undertake a new query using the main taxonomic authority (step one). 
# Note that after parsing scientific names, several names are now duplicated. In order to optimize the taxonomic standardization process, only unique names will be queried. 

# The taxonomic harmonization is based upon a taxonomic authority that users have to choose. The following taxonomic authority databases are available in taxadb package:

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

query_names <- bdc_get_taxa_taxadb(
  sci_name = database$names_parsed,
  replace_synonyms = FALSE,
  suggest_names = FALSE,
  suggestion_distance = 0.9,
  db = "gbif",
  rank_name = "Plantae",
  rank = "kingdom",
  parallel = TRUE,
  ncores = 2,
  export_accepted = FALSE
)

# Join results of taxonomic queried to database. Note that the column "original_search" containing the names parsed and the column 'verbatim_scientificName' the original names.
database <- 
  database %>% 
  dplyr::rename(verbatim_scientificName = scientificName) %>% 
  dplyr::select(-names_parsed) %>% 
  dplyr::bind_cols(., query_names)
       

# REMOVE PROBLEMATIC RECORDS ----------------------------------------------
# Before saving the database containing verified scientific names, you have to choose to remove or not names:
# - not found (i.e. unresolved names); notes = "not found"
# 1: with more than one accepted name; notes = "|more +1 accepted"
# 2: with no accepted name found; notes = "|no accepted name"
# 3: with doubtful taxonomic identification (i.e., names flagged as FALSE in the column '.taxo_uncer')

unresolved_names <-
  bdc_filter_out_names(
    data = database,
    notes = c("not_found", "more_one_accepted", "no_accepted", "taxo_uncer")
  )

# Save the table. You may want to check this table at another time
unresolved_names %>%
  data.table::fwrite(., here::here("Output", "Check", "02_unresolved_names.csv"))


# Database containing only resolved names. To select these name, used opposite == TRUE
output <-
  bdc_filter_out_names(
    data = database,
    notes = c("not_found", "more_one_accepted", "no_accepted", "taxo_uncer"), 
    opposite = TRUE
  )

# Remove unnecessary columns and save the database
bdc_filter_out_flags(data = output, columns_to_remove = .uncer_terms) %>% 
qs::qsave(., here::here("Output", "Intermediate", "02_prefilter_database.qs"))
