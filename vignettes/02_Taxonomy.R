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
# Load the database resulting from the prefilter step
prefilter_database <-
  here::here("Output", "Intermediate", "01_prefilter_database.qs") %>%
  qs::qread()

# Standardize character encoding
for (i in 1:ncol(prefilter_database)){
  if(is.character(prefilter_database[,i])){
    Encoding(prefilter_database[,i]) <- "UTF-8"
  }
}

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

# routines to clean and parse names (see the help of each function starting with "bdc" for more details)

parse_names <- 
  prefilter_database %>% 
  distinct(scientificName, .keep_all = T) %>% 
  dplyr::select(scientificName) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  dplyr::filter(!is.na(scientificName)) 

# bdc_rem_family_names: Remove family names from scientific names (e.g. Lauraceae Ocotea odorifera to Ocotea odorifera)

# bdc_rem_taxo_unc: Flag, identity, and remove taxonomic uncertainty terms (e.g. Myrcia cf. splendens to Myrcia splendens). Check ?bdc_bdc_rem_taxo_unc for a list of uncertainty terms. Infraspecific terms (variety [e.g., var.] or subspecies ([e.g. subsp.]) are not removed or flagged.)

# bdc_rem_other_issues: Remove duplicated generic names, extra spaces, and capitalize the generic name

parse_names <- 
  bdc_rem_family_names(data = parse_names, sci_names = "scientificName") %>% 
  bdc_rem_taxo_unc(data = ., sci_names = "clean_family_names") %>% 
  bdc_rem_other_issues(data = ., sci_names = "clean_uncer_terms")

# Parse names using rgnparser
gnparser <-
  parse_names %>%
  pull(clean_other_issues) %>%
  rgnparser::gn_parse_tidy() %>%
  select(verbatim, cardinality, canonicalfull, quality) %>% 
  rename(clean_other_issues = verbatim) %>% 
  rename(names_parsed = canonicalfull)

# Add names parsed
parse_names <-
  full_join(parse_names, gnparser, by = "clean_other_issues") %>% 
  distinct(scientificName, .keep_all = T)

# Match unique names parsed with to full database 
parse_names <- 
  parse_names %>%
  dplyr::full_join(prefilter_database, ., by = "scientificName")

# Save database with names parsed
parse_names %>% qs::qsave(here::here("Output", "Check", "02_parsed_names.qs"))

# Standardize taxonomic names ---------------------------------------------

# This is made in three steps. First, names are queried using a main taxonomic authority. Next, synonyms or accepted names of unresolved names are queried using a second taxonomic authority. Finally, scientific names found in step two are used to undertake a new query using the main taxonomic authority (step one). 

# Note that after parsing scientific names, several names are now duplicated. In order to optimize the taxonomic standardization process, only unique names will be queried. 

# FIXME: remover subsp. e var.
unique_sci_names <- 
  parse_names %>% 
  distinct(names_parsed, .keep_all = T) %>% # unique scientific names
  filter(!is.na(names_parsed)) # not include NAs

# Query one:
system.time({
  query_one <- bdc_get_taxa_taxadb(
    sci_name = unique_sci_names$names_parsed, # vector of names parsed
    replace.synonyms = T,
    suggest.names = T,
    suggestion.distance = 0.9,
    db = "gbif"
  )
})

# Create a vector of unresolved names, which includes names not found (i.e. NAs) and names with more than one accepted name. Note that in this first moment, this vector contains only names with more than one accepted name. Names not found will be added afterward
unresolved_names <- 
  query_one %>%
  dplyr::filter(notes == "check +1 accepted")

# Query two: (only if still remains names not found in query one)
# Search for other possible names (synonyms or accepted ones) of unresolved names using another taxonomic authority
names_NA <- 
  query_one %>%
  dplyr::filter(is.na(scientificName) & notes != "check +1 accepted")


# First, exclude unresolved names from query_one
query_one <- 
  query_one %>% 
  dplyr::filter(!is.na(scientificName) & notes != "check +1 accepted")


if (nrow(names_NA) != 0){
  system.time({
    query_two <- 
      bdc_get_taxa_taxadb(
        sci_name = names_NA$original.search, # vector of names parsed
        replace.synonyms = T,
        suggest.names = T,
        suggestion.distance = 0.9,
        db = "ncbi" # define the second taxonomic authority
      )
  })
}

# Query three: (only if previously unresolved names were resolved in query two)
# Use names retrieved from query two to carry out a new query for accepted names using the main taxonomic authority (i.e. equal to query one)

# Unresolved names are those not resolved or with more than one accepted name (or synonyms if replace_synonyms == T)

if (!is.na(query_two$scientificName) %>% any){
  system.time({
    query_three <- 
      bdc_get_taxa_taxadb(
        sci_name = query_two$scientificName,
        replace.synonyms = T,
        suggest.names = T,
        db = "gbif" # main taxonomic authority
      )
  })
  
  # CHECK: Change temporary name query by the original name
  query_three$original.search <- names_NA$original.search
  
  # In cases when at least one name was resolved...
  # Join resolved names to query one 
  query_one <- 
    query_three %>% 
    dplyr::filter(!is.na(scientificName)) %>% 
    bind_rows(query_one, .)
  
  # And join names not found to unresolved names table
  unresolved_names <- 
    query_three %>% 
    dplyr::filter(is.na(scientificName)) %>%
    bind_rows(unresolved_names, .)

  } else{
  # In cases when all names remains unresolved...
  unresolved_names <- bind_rows(unresolved_names, names_NA)
}



  

  
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
