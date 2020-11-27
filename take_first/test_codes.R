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


new_name <- new_get_names(x$acceptedNameUsageID, "fb")



# Multiple names ----------------------------------------------------------
# col e tpl nao esta funcionando, mas na do taxadb tbm nao

dat <- read.csv("./parse_names.csv", header = TRUE)
names_test <- dat %>% pull(input_cleaned)
txdb <- filter_name(names_test, "gbif")

res <- new_get_names(txdb$acceptedNameUsageID, "tpl")
res <- taxadb::get_names(txdb$acceptedNameUsageID, "tpl")

names_test[is.na(res)]


