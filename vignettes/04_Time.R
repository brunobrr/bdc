# LOAD ALL FUNCTION OS BDC WORKFLOW ---------------------------------------
devtools::load_all()

# INSTALL AND LOAD PACKAGES REQUERIED -------------------------------------
ipak(
  c(
    "tidyverse",
    "here",
    "dplyr", 
    "qs"
  )
)

# CREATE DIRECTORIES ------------------------------------------------------
# Create directories for saving the results. If not existing, four new folders will be created in the folder 'Output'
bdc_create_dir()

# LOAD THE DATABASE -------------------------------------------------------
# Load the database resulting from the prefilter step or your own database
database <-
  here::here("Output", "Intermediate", "03_space_database.qs") %>%
  qs::qread()

# Standardize characters encoding
for (i in 1:ncol(database)){
  if(is.character(database[,i])){
    Encoding(database[,i]) <- "UTF-8"
  }
}

# PARSING DATE ------------------------------------------------------------
parse_date <-
  bdc_parse_date(data = database,
                 col_to_test = "verbatimEventDate",
                 year_threshold = 1500)


# Mapping spatial errors --------------------------------------------------
# Mapping a column containing the results of the temporal test
parse_date %>%
  dplyr::filter(.year == FALSE) %>%
  bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".year",
    size = 0.5
  )

# REPORT ------------------------------------------------------------------
# Create a summary column. This column is FALSE if any test was flagged as FALSE (i.e. potentially invalid or problematic record)
parse_date <- bdc_summary_col(data = parse_date)


# Create a report summarizing the results of all tests
report <- bdc_create_report(data = parse_date, workflow_step = "temporal")

# FIGURES -----------------------------------------------------------------
bdc_create_figures(data = parse_date, workflow_step = "time")

# CLEAN THE DATABASE ------------------------------------------------------
# Removing flagged records (potentially problematic ones) and saving a 'clean' database (i.e., without columns of tests, which starts with ".")
output <-
  parse_date %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc_filter_out_flags(data = ., col_to_remove = "all")

output %>% 
  qs::qsave(., here::here("Output", "Intermediate", "04_time_database.qs"))

