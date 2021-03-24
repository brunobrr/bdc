# LOAD ALL FUNCTION OS BDC WORKFLOW ---------------------------------------
devtools::load_all()

# INSTALL AND LOAD PACKAGES REQUERIED -------------------------------------
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

# CREATE DIRECTORIES ------------------------------------------------------
# Create directories for saving the results. If not existing, four new folders
# will be created in the folder 'Output'.
bdc_create_dir()

# LOAD THE DATABASE -------------------------------------------------------
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
# Records empty scientific name
check_pf1 <- bdc_scientificName_empty(
  data = database,
  sci_name = "scientificName")

# CHECK 2 -----------------------------------------------------------------
# Records empty latitude or longitude
check_pf2 <- bdc_coordinates_empty(
  data = check_pf1,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

# CHECK 3 -----------------------------------------------------------------
# Records with out-of-range coordinates (longitude between -180 and
# 180; latitude between -90 and 90)
check_pf3 <- bdc_coordinates_outOfRange(
  data = check_pf2,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

# CHECK 4 -----------------------------------------------------------------
# Records from doubtful provenance
check_pf4 <- bdc_basisOfRrecords_notStandard(
  data = check_pf3,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")

# CHECK 5 -----------------------------------------------------------------
# Getting country names from coordinates for records missing country names
check_pf5 <- bdc_country_from_coordinates(
  data = check_pf4,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country")

# CHECK 6 -----------------------------------------------------------------
# Standardizing country names and getting country code information
check_pf6 <- bdc_country_standardized(
  data = check_pf5,
  country = "country"
)

# CHECK 7 -----------------------------------------------------------------
# Correcting latitude and longitude transposed
check_pf7 <-
  bdc_coordinates_transposed(
    data = check_pf6, 
    id = "database_id",
    sci_names = "scientificName",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    country = "country"
  )

# CHECK 8 -----------------------------------------------------------------
# Records outside one or multiple reference countries (e.g. exclude records in
# other countries or far from a informed distance from the coast)
check_pf8 <-
  bdc_coordinates_country_inconsistent(
    data = check_pf7,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees (~11 km at the equator)
  )

# CHECK 9 -----------------------------------------------------------------
# Save records with empty or out-of-range coordinates but with
# potentially valid information about the collecting locality.
check_pf9 <-
  bdc_coordinates_from_locality(
    data = check_pf8,
    locality = "locality",
    lon = "decimalLongitude",
    lat = "decimalLatitude"
  )

# REPORT ------------------------------------------------------------------
# Creating a summary column. This column is "FALSE" if any data quality test was
# flagged as FALSE
# (i.e. potentially invalid or problematic record)
check_pf10 <- bdc_summary_col(data = check_pf9)

# Creating a report summarizing the results of all tests
report <-
  bdc_create_report(data = check_pf10,
                    database_id = "database_id",
                    workflow_step = "prefilter")
report

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = check_pf10,
                   database_id = "database_id",
                   workflow_step = "prefilter")

# FILTER THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean'
# database (i.e., without test columns starting with ".")
output <-
  check_pf10 %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "01_prefilter_database.qs"))
