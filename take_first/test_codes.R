library(taxadb)
library(dplyr)
library(duckdb)
library(flora)

source(here::here("take_first/new_functions.R"))

# Download fb database ----------------------------------------------------

td_create("fb")


# Get species data --------------------------------------------------------

x <- filter_name("Leporinus reinhardti", "fb")

# Return duplicates -------------------------------------------------------

dup <- filter_id(x$acceptedNameUsageID, "fb")

right_name <- dup %>% filter(taxonomicStatus == "accepted") %>% pull(scientificName)

# Testing take_first ------------------------------------------------------


max_name <- max_get_names(x$acceptedNameUsageID, "fb")

min_name <- min_get_names(x$acceptedNameUsageID, "fb")

c(max_name, min_name) == right_name
