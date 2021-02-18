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
# Create directories for saving the results. If not existing, four new folders will be created in the folder 'Output'.
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
# Flag records outside one or multiple focal countries (e.g. exclude records in other countries or far from a informed distance from the coast)
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
bdc_tests_summary(data = data_pf7, workflow_step = "prefilter")

# Save the report
bdc_tests_summary(data = data_pf7, workflow_step = "prefilter") %>% 
  data.table::fwrite(., here::here("Output/Report/01_Prefilter_Report.csv"))

# Save records with invalid or missing coordinates but with information potentially valid about the locality from which coordinates information can be extracted
data_to_check <-
  bdc_xy_from_locality(
    data = data_pf7,
    locality = "locality",
    lon = "decimalLongitude",
    lat = "decimalLatitude"
  )

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = data_pf7, workflow_step = "prefilter")

# CLEAN THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean' database (i.e., without columns of tests, which starts with ".")
output <-
  data_pf7 %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., columns_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "01_prefilter_database.qs"))
