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


# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))
fs::dir_create(here::here("Output/Report"))
fs::dir_create(here::here("Output/Figures"))


# Load database -----------------------------------------------------------
# Load the database resulting from the prefilter step or your own database
database <-
  here::here("Output", "Intermediate", "01_prefilter_database.qs") %>%
  qs::qread()

# Standardize character encoding
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}


# Parse scientific names --------------------------------------------------

# routines to clean and parse names (see the help of each function starting with "bdc" for more details)

# Summary of each test:

# bdc_rem_family_names: Remove family names from scientific names (e.g. Felidae Panthera onca to Panthera onca; Lauraceae Ocotea odorifera to Ocotea odorifera)

# bdc_rem_taxo_unc: Flag, identity, and remove taxonomic uncertainty terms (e.g. Myrcia cf. splendens to Myrcia splendens). Check ?bdc_bdc_rem_taxo_unc for a list of uncertainty terms and their ortographycal varations. 

# bdc_rem_other_issues: Convert to lower case and capitalize the only first letter of the generic names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae) and remove extra spaces

# bdc_rem_infaesp_names: Flag, identity, and remove infraspecific terms (subspecies, varietas and forma)

# bdc_gnparser: Extract just binomial scientific names (without year or authors). To do this, a scientific name is breaks down in different components using rgnparser package.

# Select unique scientific names
uniq_sciNames <- 
  database %>% 
  dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
  dplyr::select(scientificName) %>% # select this column
  dplyr::mutate_all(na_if,"") %>% # change white to NA
  dplyr::filter(!is.na(scientificName)) # remove NAs

# Parse names
parse_names <- 
  bdc_rem_family_names(data = uniq_sciNames, sci_names = "scientificName") %>% 
  bdc_rem_taxo_unc(data = ., sci_names = "clean_family_names") %>% 
  bdc_rem_other_issues(data = ., sci_names = "clean_uncer_terms")  %>% 
  bdc_rem_infaesp_names(data = ., sci_names = "clean_other_issues") %>% 
  bdc_gnparser(data = ., sci_names = "clean_infaesp_names")

# Save database with names parsed
temp <- database %>% dplyr::select(scientificName)
parse_names %>%
  dplyr::full_join(temp, ., by = "scientificName") %>% 
  qs::qsave(here::here("Output", "Check", "02_parsed_names.qs"))

# Merge unique names parsed to full database and save the output of the parsing names process. Note that only the column "names_parsed" will be used in the downstream analyses. The outputs of each step of the parsing names process can be checked in "Output/Check/02_parsed_names.qs"
database <- 
  parse_names %>%
  dplyr::select(scientificName, names_parsed) %>% 
  dplyr::full_join(database, ., by = "scientificName")


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


# To optimize the process, only unique scientific names retrieved from the parsing names process will be queried.
uni_parse_names <- 
  database %>% 
  distinct(names_parsed, .keep_all = T) %>% # unique scientific names
  filter(!is.na(names_parsed)) # not include names NAs

# Query one:
system.time({
  query_one <- bdc_get_taxa_taxadb(
    sci_name = uni_parse_names$names_parsed, # vector of names parsed
    replace.synonyms = T,
    suggest.names = T,
    suggestion.distance = 0.9,
    db = "gbif",
    rank_name = "Plantae", 
    rank = "kingdom",
    parallel = T,
    ncores = 5
  )
})

# FIXME: How merge query_one containing more names than database? (in cases when replace_synomyn = F)
# Join resolved names to query one 
teste <- 
  query_three %>% 
  dplyr::filter(!is.na(scientificName)) %>% 
  bind_rows(query_one, .)


# Create a vector of unresolved names, which includes names not found (i.e. NAs) and names with more than one accepted name. Note that in this first moment, this vector contains only names with more than one accepted name. Names not found will be added afterward
unresolved_names <- 
  query_one %>%
  dplyr::filter(is.na(scientificName) | notes %in% c("check +1 accepted", "|check no accepted name"))



  
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
