# LOAD ALL FUNCTION OS BDC WORKFLOW ---------------------------------------
devtools::load_all()

# INSTALL AND LOAD PACKAGES REQUERIED -------------------------------------
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

# CREATE DIRECTORIES ------------------------------------------------------
# Create directories for saving the results. If not existing, four new folders will be created in the folder 'Output'
bdc_create_dir()

# LOAD THE DATABASE -------------------------------------------------------
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


# CLEAN AND PARSE NAMES ---------------------------------------------------
# Routines to clean and parse names, including tests for:
# 1 - remove family names from scientific names (e.g. Felidae Panthera onca to Panthera onca; Lauraceae Ocotea odorifera to Ocotea odorifera)

# 2 - Flag, identify, and remove taxonomic uncertainty terms (e.g. Myrcia cf. splendens to Myrcia splendens). Check ?bdc_clean_names for a list of terms denoting taxonomic uncertainty and their orthographic variations

# 3 - Convert to lower case and capitalize the only first letter of the generic names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae) and remove extra spaces

# 4 - Flag, identify, and remove infraespecific terms (subspecies, variety and forma)

# 5 - Extract just binomial scientific names (without year or authors). To do this, a scientific name is breaks down in different components using rgnparser package

parse_names <- bdc_clean_names(sci_names = database$scientificName)

# Save the results of the parsing names process
parse_names %>%
  data.table::fwrite(., here::here("Output", "Check", "02_parsed_names.csv"))

# Join names parsed to full database. Note that only the column "names_clean" will be used in the downstream analyses.
database <- 
  parse_names %>%
  dplyr::select(scientificName, .uncer_terms, .infraesp_names, names_clean) %>% 
  dplyr::full_join(database, ., by = "scientificName")


# FIXME: delete this file 
database <- qs::qread("database_exe_.qs")
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}


# STANDARDIZE NAMES -------------------------------------------------------
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
       
# REPORT ------------------------------------------------------------------

# FIGURES -----------------------------------------------------------------


# CLEAN THE DATABASE ------------------------------------------------------
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
