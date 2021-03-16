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
    "rworldmap", # Check this
    "flora", 
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

# continent_border <-
#   rnaturalearth::ne_download(scale = "large",
#                              type = 'land',
#                              category = 'physical')

# Just in case the 'prefilter' step were not executed
database <- cc_val(
  x =  database,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  value = "clean"
)

flag_issues <-
  CoordinateCleaner::clean_coordinates(
    x =  database,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",
      "centroids",
      "duplicates",
      "equal",
      "gbif",
      "institutions",
      "outliers",
      # "seas", # Check whether this is necessary
      "zeros",
      "urban"
    ),
    capitals_rad = 10000,
    centroids_rad = 1000,
    centroids_detail = "both",
    inst_rad = 100,
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 7,
    #acho que esse aqui podemos aumentar visando que existem muitas esp?ceis pouco amostradas
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "iso_a3",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid"
  )

# FiXME: add cc_coun and create bdc_out_range

#  Flagging low decimal precision 
flag_issues <-
  bdc_xy_precision(
    data = flag_issues,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = c(0, 1, 2) #number of decimals to be tested
  )

# Mapping spatial errors --------------------------------------------------
# Mapping a column containing the results of one spatial test
flag_issues %>%
  dplyr::filter(.cen == FALSE) %>%
  bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".cen",
    size = 1
  )

# REPORT ------------------------------------------------------------------
# Create a summary column. This column is FALSE if any test was flagged as FALSE (i.e. potentially invalid or problematic record)
flag_issues <- bdc_summary_col(data = flag_issues)

# Create a report summarizing the results of all tests
report <- bdc_create_report(data = flag_issues, workflow_step = "space")

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = flag_issues, workflow_step = "space")

# CLEAN THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean' database (i.e., without columns of tests, which starts with ".")
output <-
  flag_issues %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "03_space_database.qs"))
