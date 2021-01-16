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
    "stringr",
    "flora"
  )
)

# FIXME: Check if all directories are needed

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))
fs::dir_create(here::here("Output/Report"))
fs::dir_create(here::here("Output/Figures"))


# Load database -----------------------------------------------------------
# Load the database resulting from the prefilter step
prefilter_database <-
  here::here("Output", "Intermediate", "01_prefilter_database.qs") %>%
  qs::qread()

sci_names <-
  prefilter_database %>%
  dplyr::pull(scientificName)


# Download databases ------------------------------------------------------
# Taxonomic authority

# this is one-time setup used to download, extract and import taxonomic database from the taxonomic authority defined by the user (see Norman et al. 2020 in Methods in Ecology and Evolution). Options currently recognized in taxadb are:

# Select one taxonomic authority.
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

taxo_authority <- "gbif"
taxadb::td_create(taxo_authority, schema = "dwc", overwrite = FALSE)

# rngparser

# one-time setup to download and install rgnparser, which is used to parse scientific name (for more details, see https://github.com/ropensci/rgnparser)
rgnparser::install_gnparser(force = F)


# Parse scientific names --------------------------------------------------

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


# routines to clean and parse names (see the help of each function starting with "bdc" for more details)

parse_names <- df

# remove family names from scientific names
rem_family <-
  parse_names %>% 
  pull(input) %>%
  bdc_rem_family_names()

parse_names <- 
  parse_names %>% 
  mutate(clean_family_names = rem_family$sp_names) %>% 
  mutate(flag_family_names = rem_family$flag_family)

# Remove taxonomic uncertainty terms (e.g. sp, afins, cf)
rem_uncer <- 
  parse_names %>% 
  pull(clean_family_names) %>%
  bdc_rem_taxo_unc()

parse_names <- 
  parse_names %>% 
  mutate(clean_uncer_terms = rem_uncer)

# Flag taxonomic uncertainty terms
flag_uncer<- 
  parse_names %>% 
  pull(clean_family_names) %>%
  bdc_flag_taxo_unc()

parse_names <- 
  parse_names %>% 
  mutate(uncer_terms = flag_uncer$term_uncertainty) %>% 
  mutate(flag_uncer_terms = flag_uncer$taxo_uncertainty)

# Remove duplicated generic names, extra spaces, and capitalize the generic name
other_issues <-
  parse_names %>% 
  pull(clean_uncer_terms) %>% 
  bdc_rem_other_issues()

parse_names <- 
  parse_names %>% 
  mutate(clean_other_issues = other_issues) 
# rename(input_cleaned = clean_other_issues)

# Parse names using rgnparser 
gnparser <-
  parse_names %>%
  pull(clean_other_issues) %>%
  rgnparser::gn_parse_tidy() %>%
  select(verbatim, cardinality, canonicalfull, quality) %>% 
  rename(clean_other_issues = verbatim) %>% 
  rename(input_parsed = canonicalfull)

# Names parsed
parse_names <-
  full_join(parse_names, gnparser, by = "clean_other_issues") %>% 
  distinct(temp_id, .keep_all = T)

# Save the file
parse_names %>%
  dplyr::select(input, temp_id, input_parsed) %>%
  dplyr::full_join(., df0, by = c("input", "temp_id")) %>%
  data.table::fwrite(here::here("Output", "Check", "02_parsed_names.csv"))


# Standardize taxonomic names ---------------------------------------------

# This is made in three steps. Names are queried using a unique taxonomic authority. Then, synonyms or accepted names of Unresolved names are queried using a second taxonomic authority. Finally, scientific names found in step two are used to undertaken a new query using the main taxonomic authority. 

system.time({
query_one <- bdc_get_taxa_taxadb(
  sci_name = a,
  replace.synonyms = T,
  suggest.names = T,
  db = "gbif"
)
})

# Search for another possible names (synonyms or accepted ones) of unresolved names using another taxonomic authority (GBIF is the default taxonomic authority)



# Unresolved names are those not resolved or with more than one accepted name
unresolved_names <- 
  query_one %>%
  dplyr::filter(is.na(scientificName) & notes != "check +1 accepted")


# Search for another possible names (synonyms or accepted ones) of unresolved names using other taxonomic authority
names_to_query <- 
  query_one %>%
  dplyr::filter(is.na(scientificName) & notes != "check +1 accepted")

if (nrow(names_to_query) != 0){
  query_two <- bdc_get_taxa_taxadb(
    sci_name = names_to_query$original.search, # change this
    replace.synonyms = T,
    suggest.names = T,
    db = "itis"
  )
}

# Use names retrieved from WFO to carry out a new query for accepted names in taxadb


# Unresolved names are those not resolved or with more than one accepted name (or synonyms if replace_synonyms == T)


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
