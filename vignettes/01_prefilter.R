# Load all functions of BDC workflow
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

# Load data ---------------------------------------------------------------
# Load the merge database
###########  how add locale information?
merged <-
  here::here("data", "temp", "standard_database.qs") %>%
  qs::qread()


# CHECK 1 -----------------------------------------------------------------
# Flag records missing scientific name (i.e empty or NA)
data_pf1 <-
  merged %>%
  dplyr::mutate(.missing_name =
                  bdc_flag_missing_names(.,
                                         sci_name = "scientificName"))
# CHECK 2 -----------------------------------------------------------------
# Flag records missing latitude or longitude 
data_pf2 <-
  data_pf1 %>%
  dplyr::mutate(.missing_xy =
                  bdc_flag_missing_xy(.,
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude"))
# CHECK 3 -----------------------------------------------------------------
# Flag records with invalid coordinates
data_pf3 <-
  data_pf2 %>%
  dplyr::mutate(.invalid_xy =
                  bdc_flag_invalid_xy(., 
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude"))

# CHECK 4 -----------------------------------------------------------------
# Flag records from doubtful provenance
data_pf4 <-
  data_pf3 %>%
  dplyr::mutate(.xy_provenance =
                  bdc_flag_xy_provenance(.,
                                         basisOfRecord = "basisOfRecord"))

# CHECK 5 -----------------------------------------------------------------
# Correct latitude and longitude transposed
data_pf5 <-
  data_pf4 %>% bdc_flag_transposed_xy(
    data = .,
    id = "database_id",
    sci_name = "scientificName",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    country = "country"
  )

# CHECK 6 -----------------------------------------------------------------
# Flag records outside the focal country (e.g. in the ocean or in other countries)
data_pf6 <-
  data_pf5 %>%
  dplyr::mutate(.xy_out_country = bdc_flag_xy_out_country(
    data = .,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.5
  ))

# REPORT ------------------------------------------------------------------
# Create a summary column. This column is FALSE if any test was flagged as FALSE (i.e. potentially invalid or problematic record)
data_pf6 <- 
  data_pf6 %>% 
  mutate(.summary = bdc_summary_col(.))
  
# Create a report summarizing the results of all tests
bdc_tests_summary(data = data_pf6)

# View the report
bdc_tests_summary(data = data_pf6) 

# Save the report
bdc_tests_summary(data = data_pf6) %>% 
  data.table::fwrite(., here::here("Output/Report/01_Report.csv"))

# Save records missing or with invalid coordinates but with information on locality 
########### FIXME: remove cells with only special characters (".", ",", " ", etc)
data_to_check <-
  bdc_xy_from_locality(
    data = data_pf6,
    locality = "locality",
    lon = "decimalLongitude",
    lat = "decimalLatitude"
  )


#TODO: Save figures

# REMOVE PROBLEMATIC RECORDS ----------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a clean database
bdc_filter_out_flags(data = data_pf6) %>% 
  qs::qsave(., here::here("Output/Intermediate/01_database"))
  

