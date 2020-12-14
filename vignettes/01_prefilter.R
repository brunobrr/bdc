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
    "rnaturalearth"
  )
)

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))

# Load the merge database

# FIXEME: how add locale information?
merged <-
  here::here("data", "temp", "standard_database.qs") %>%
  qs::qread()

utils::type.convert()

# CHECK 1: Flag records missing scientific name (i.e empty or NA)
data_pf1 <-
  merged %>%
  dplyr::mutate(.missing_name =
                  bdc_flag_missing_names(.,
                                         sci_name = "scientificName"))

# CKECK 2: Flag records missing latitude or longitude 
data_pf2 <-
  data_pf1 %>%
  dplyr::mutate(.missing_xy =
                  bdc_flag_missing_xy(.,
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude"))

# CHECK 3: Flag records with invalid coordinates
data_pf3 <-
  data_pf2 %>%
  dplyr::mutate(.invalid_xy =
                  bdc_flag_invalid_xy(., 
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude"))

# CKECK 4: Flag records from doubtful provenance
data_pf4 <-
  data_pf3 %>%
  dplyr::mutate(.xy_provenance =
                  bdc_flag_xy_provenance(.,
                                         basisOfRecord = "basisOfRecord"))

# CHECK 5. Correct latitude and longitude transposed
data_pf5 <-
  data_pf4 %>% bcd_flag_transposed_xy(
    data = .,
    id = "database_id",
    sci_name = "scientificName",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    country = "country"
  )

# CHECK 6. Records outside the focal country (e.g. records in the ocean or in other countries)
data_pf6 <-
  data_pf5 %>%
  dplyr::mutate(.xy_out_country = bdc_flag_xy_out_country(
    data = .,
    country_name = "Brazil",
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    dist = 0.5
  ))

 
# CHECK: 7. Save records missing or with invalid coordinates but with information on locality 

# FIXME: remove cells with only special characters (".", ",", " ", etc)
  data_pf6 %>%
  filter(locality != "",
         !is.na(locality), 
         .missing_xy == FALSE, 
         .invalid_xy == FALSE) %>% 
  data.table::fwrite(here::here("Output", "Check", "01_prefilter_no_coordinates_but_locality.csv"))

bdc_filter_out_flags(data_pf6) %>%
teste <- bdc_check_flags(data_pf6)
 

