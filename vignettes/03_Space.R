# LOAD ALL FUNCTION OS BDC WORKFLOW ---------------------------------------
devtools::load_all()

# INSTALL AND LOAD PACKAGES REQUERIED -------------------------------------
ipak(
  c(
    "tidyverse",
    "here",
    "dplyr", 
    "rnaturalearth",
    "CoordinateCleaner",
    "stringr",
    "qs"
  )
)

# CREATE DIRECTORIES ------------------------------------------------------
# Create directories for saving the results. If not existing, four new folders will be created in the folder 'Output'
bdc_create_dir()

# LOAD THE DATABASE -------------------------------------------------------
# Load the database resulting from the prefilter step or your own database
database <-
  here::here("Output", "Intermediate", "02_taxonomy_database.qs") %>%
  qs::qread()

# Standardize characters encoding
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}

# FLAGGING COMMON SPATIAL ERRORS ------------------------------------------
# Flagging potentially erroneous coordinates using test avaiable in the 'CoordinateCleaner' package.

check_space01 <-
  CoordinateCleaner::clean_coordinates(
    x =  database,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # remove country and province centroids within 2km
      "centroids",    # remove capitals centroids within 2km
      "duplicates",   # remove duplicated records
      "equal",        # remove equal coordinates
      "gbif",         # remove in GBIF headsquare
      "institutions", # remove zoo and herbaria within 2km 
      "outliers",     # remove outliers
    # "seas",         # remove from ocean 
      "zeros",        # remove coordinates 0,0
      "urban"         # remove urban areas
    ),
    capitals_rad = 2000, 
    centroids_rad = 2000, 
    centroids_detail = "both",
    inst_rad = 100, # remove zoo and herbaria within 2km 
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid"
  )

#  Remove low decimal precision 
check_space02 <-
  bdc_coordinates_precision(
    data = check_space01,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = c(0, 1, 2) #number of decimals to be tested
  )

# Mapping spatial errors --------------------------------------------------
# Mapping a column containing the results of one spatial test
check_space02 %>%
  dplyr::filter(.cen == FALSE) %>%
  bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".cen",
    size = 1
  )

# REPORT ------------------------------------------------------------------
# Creating a summary column. This column is "FALSE" if any data quality test was
# flagged as FALSE
# (i.e. potentially invalid or problematic record)
check_space03 <- bdc_summary_col(data = check_space02)

# Creating a report summarizing the results of all tests
report <-
  bdc_create_report(data = check_space03,
                    database_id = "database_id",
                    workflow_step = "prefilter")
report

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = check_space03,
                   database_id = "database_id",
                   workflow_step = "space")

# FILTER THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean'
# database (i.e., without test columns starting with ".")
output <-
  flag_issues %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "03_space_database.qs"))
