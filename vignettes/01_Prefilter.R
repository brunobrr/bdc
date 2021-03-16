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
# Identify records missing scientific name (i.e empty or NA records)
data_pf1 <- bdc_missing_names(
  data = database,
  sci_name = "scientificName")

# CHECK 2 -----------------------------------------------------------------
# Identify records missing latitude or longitude (i.e empty or NA records)
data_pf2 <- bdc_missing_coordinates(
  data = data_pf1,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

# CHECK 3 -----------------------------------------------------------------
# Identify records with invalid coordinates (lat > 90 or -90; long >180 or -180;
# coordinates NA)
data_pf3 <- bdc_invalid_coordinates(
  data = data_pf2,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

# CHECK 4 -----------------------------------------------------------------
# Identify records from doubtful provenance
data_pf4 <- bdc_invalid_basis_of_records(
  data = data_pf3,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")

# CHECK 5 -----------------------------------------------------------------
# Gets country names from coordinates for records missing country names
data_pf5 <- bdc_countryName_from_coordinates(
  data = data_pf4,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country")

# CHECK 6 -----------------------------------------------------------------
# Standardizes country names and gets country code information
data_pf6 <- bdc_standardize_countryNames(
  data = data_pf5,
  country = "country"
)

# CHECK 7 -----------------------------------------------------------------
# Correct latitude and longitude transposed
data_pf7 <-
  bdc_transposed_coordinates(
    data = data_pf6, 
    id = "database_id",
    sci_names = "scientificName",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    country = "country"
  )

# CHECK 8 -----------------------------------------------------------------
# Flag records outside one or multiple focal countries (e.g. exclude records in
# other countries or far from a informed distance from the coast)
data_pf8 <-
  bdc_coordinates_out_country(
    data = data_pf7,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.1 # in decimal degrees
  )

# CHECK 9 -----------------------------------------------------------------
# Save records with invalid or missing coordinates but with information
# potentially valid about the locality from which coordinates information can be
# extracted
data_to_check <-
  bdc_coordinates_from_locality(
    data = data_pf8,
    locality = "locality",
    lon = "decimalLongitude",
    lat = "decimalLatitude"
  )

# REPORT ------------------------------------------------------------------
# Create a summary column. This column is FALSE if any test was flagged as FALSE
# (i.e. potentially invalid or problematic record)
data_pf9 <- bdc_summary_col(data = data_pf8)

# Create a report summarizing the results of all tests
report <-
  bdc_create_report(data = data_pf9,
                    databa_id = "databas_id",
                    workflow_step = "prefilter")
report

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = data_pf9,
                   databa_id = "databas_id",
                   workflow_step = "prefilter")

# FILTER THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean'
# database (i.e., without columns of tests, which starts with ".")
output <-
  data_pf9 %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "01_prefilter_database.qs"))
