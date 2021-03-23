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
parse_names <-  qs::qread(here::here("Output", "Check", "02_parsed_names.qs"))

parse_names <- bdc_clean_names(sci_names = database$scientificName)


# Merge names parsed with the full database. As the column 'scientificName' is in the same order in both databases (i.e., parse_names and database), we can append names parsed in the database. Also, only the columns "names_clean" and ".uncert_terms" will be used in the downstream analyses. But don't worry, you can check the results of parsing names process in "Output/Check/02_parsed_names.qs"
parse_names <- 
  parse_names %>%
  dplyr::select(.uncer_terms, names_clean)

database <- dplyr::bind_cols(database, parse_names)

# FIXME: delete this file 
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}

# STANDARDIZE NAMES -------------------------------------------------------
# The taxonomic harmonization is based upon a taxonomic authority that users have to choose. The following taxonomic authority databases available in taxadb package are:

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

query_names <- bdc_query_names_taxadb(
  sci_name = database$names_clean,
  replace_synonyms = TRUE,
  suggest_names = TRUE,
  suggestion_distance = 0.9,
  db = "gbif",
  rank_name = "Plantae",
  rank = "kingdom",
  parallel = TRUE,
  ncores = 2,
  export_accepted = FALSE
)

# Merge results of taxonomy standardization process with the 'database.' To simplicity, let's rename the original name to "verbatim_scientificName". From now on "scientifiName" will refers to the verified names (resulted from standardization process). As the column "original_search" in "query_names" and "names_clean" are equal, only the first will be kept.
database <- 
  database %>% 
  dplyr::rename(verbatim_scientificName = scientificName) %>% 
  dplyr::select(-names_clean) %>% 
  dplyr::bind_cols(., query_names)

# REPORT ------------------------------------------------------------------
# Create a report summarizing the main results of the harmonizing names process
report <-
  bdc_create_report(data = database,
                    database_id = "database_id",
                    workflow_step = "taxonomy")
report
# CLEAN THE DATABASE ------------------------------------------------------
# As a general guidance, we did not recommend the removal of records after the prefilter step. However, it also possible filter out records with taxonomic status different of "accepted". Moreover, you can listed all taxonomic status you would like to keep. For this, all taxonomic status have to be listed in the argument 'taxonomic_notes'. 

unresolved_names <-
  bdc_filter_out_names(data = database,
                       taxonomic_notes = "accepted",
                       opposite = TRUE)

# Save the table. You may want to check this table at another time
unresolved_names %>%
  data.table::fwrite(., here::here("Output", "Check", "02_unresolved_names.csv"))

# Database containing only resolved names. To select these name, used opposite == TRUE
output <-
  bdc_filter_out_names(
    data = database,
    taxonomic_notes = "accepted", 
    opposite = FALSE
  )

# Remove unnecessary columns and save the database
bdc_filter_out_flags(data = output, col_to_remove = ".uncer_terms") %>% 
  qs::qsave(., here::here("Output", "Intermediate", "02_taxonomy_database.qs"))
