# Load all functions of bdc workflow
devtools::load_all()

# Install and load packages
ipak(
  c(
    "tidyverse",
    "here",
    "fs",
    "vroom",
    "CoordinateCleaner",
    "rnaturalearth",
    "dplyr",
    "xml2",
    "rvest",
    "qs", 
    "sf", 
    "rnaturalearth",
    "data.table"
  )
)

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))
fs::dir_create(here::here("Output/Report"))
fs::dir_create(here::here("Output/Figures"))

# Load data ---------------------------------------------------------------
# Load the merged database
database <-
  here::here("Output", "Intermediate", "00_merged_database.qs") %>%
  qs::qread()

# Standardize character encoding
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}

# CHECK 1 -----------------------------------------------------------------
# Flag records missing scientific name (i.e empty or NA records)
data_pf1 <- 
  bdc_flag_missing_names(data = database,
                         sci_name = "scientificName")

# CHECK 2 -----------------------------------------------------------------
# Flag records missing latitude or longitude (i.e empty or NA records)
data_pf2 <-
  bdc_flag_missing_xy(data = data_pf1,
                      lon = "decimalLongitude",
                      lat = "decimalLatitude")
# CHECK 3 -----------------------------------------------------------------
# Flag records with invalid coordinates (lat > 90 or -90; long >180 or -180; coordinates NA)
data_pf3 <-
  bdc_flag_invalid_xy(data = data_pf2,
                      lon = "decimalLongitude",
                      lat = "decimalLatitude")

# CHECK 4 -----------------------------------------------------------------
# Flag records from doubtful provenance
data_pf4 <-
  bdc_flag_xy_provenance(data = data_pf3,
                         basisOfRecord = "basisOfRecord")

# CHECK 5 -----------------------------------------------------------------
# Correct latitude and longitude transposed
data_pf5 <-
  bdc_flag_transposed_xy(
    data = data_pf4,
    id = "database_id",
    sci_name = "scientificName",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    country = "country"
  )

# CHECK 6 -----------------------------------------------------------------
# Flag records outside the focal country (e.g. in the ocean or in other countries)
data_pf6 <-
  bdc_flag_xy_out_country(
    data = data_pf5,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.5 # in decimal degrees
  )

# REPORT ------------------------------------------------------------------
# Create a summary column. This column is FALSE if any test was flagged as FALSE (i.e. potentially invalid or problematic record)
data_pf7 <- bdc_summary_col(data = data_pf6)
  
# Create a report summarizing the results of all tests
bdc_tests_summary(data = data_pf7)

# Save the report
bdc_tests_summary(data = data_pf7) %>% 
  data.table::fwrite(., here::here("Output/Report/01_Report.csv"))

# Save records with invalid or missing coordinates but with information on the locality 
data_to_check <-
  bdc_xy_from_locality(
    data = data_pf7,
    locality = "locality",
    lon = "decimalLongitude",
    lat = "decimalLatitude"
  )

# Create and save figures
bdc_create_figures(data = data_pf7, tests = NULL, workflow_step = "prefilter")

# REMOVE PROBLEMATIC RECORDS ----------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a clean database (without columns starting with ".")
bdc_filter_out_flags(data = data_pf7, rem_summary = TRUE) %>%
  qs::qsave(., here::here("Output", "Intermediate", "01_prefilter_database.qs"))
