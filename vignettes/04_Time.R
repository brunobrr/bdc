
# Standardize temporal information  ---------------------------------------

# install and load packages
ipak(
  c(
    "tidyverse",
    "vroom",
    "here",
    "dplyr", 
  )
)

# Load all functions of BDC workflow
devtools::load_all()

# Create a directory to save output files
fs::dir_create(here::here("Output/Check"))

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


