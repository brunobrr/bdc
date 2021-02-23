# LOAD ALL FUNCTION OS BDC WORKFLOW ---------------------------------------
devtools::load_all()

# INSTALL AND LOAD PACKAGES REQUERIED -------------------------------------
ipak(
  c(
    "tidyverse",
    "vroom",
    "here",
    "dplyr", 
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

# FiXEME: Change database

# Load the database
data_to_load <- here::here("data", "temp", "standard_database.xz")
df <- vroom(data_to_load)

parse_date <-
  bdc_parse_date(x = df,
                 column_to_test = "eventDate",
                 year_threshold = 1500)


# FiXEME: update summary?

# Save the table
parse_date %>%
  data.table::fwrite(here::here("Output", "Intermediate", "04_Time.csv"))


