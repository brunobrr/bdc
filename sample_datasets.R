### Packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

ipak(c("tidyverse", "data.table", "vroom", "data.table"))




# atlantic epiphytes ------------------------------------------------------

# atlantic epiphytes - occurence records
atlant_occur <-
  vroom(
    "Data_raw/at_epiphytes/DataS1_Occurrence.txt",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Atlantic epiphytes - abundance data
atlant_abund <-
  vroom(
    "Data_raw/at_epiphytes/DataS1_Abundance.txt",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Filter collumn names according to collumn names of "atlantic_occur"
names <- which(colnames(atlant_abund) %in% colnames(atlant_occur))
atlant_abund <-  atlant_abund[, names]

atlant_occur %>%  names  %>%  sort
atlant_abund %>%  names  %>%  sort

# Are identifical?
identical(atlant_occur %>% names %>% sort,
          atlant_abund %>% names %>% sort)

# Merge datasets
at_epiphytes <- rbind(atlant_occur, atlant_abund)

at_epiphytes_s <- sample_n(at_epiphytes, 10000)

# Save file
data.table::fwrite(at_epiphytes_s, "Input_files/at_epiphytes.csv")



# bien --------------------------------------------------------------------
bien <-
  vroom(
    "Data_raw/bien/BIEN_Brazil-UTF8.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Select a sub-sample
bien_s <- sample_n(bien, 10000)

# Save file
data.table::fwrite(bien_s, "Input_files/bien.csv")



# dryflor -----------------------------------------------------------------
dryflor <-
  vroom(
    "Data_raw/dryflor/records.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Select a sub-sample
dryflor_s <- sample_n(dryflor, 10000)

# Save file
data.table::fwrite(dryflor_s, "Input_files/dryflor.csv")



# gbif --------------------------------------------------------------------
gbif <-
  vroom(
    "Data_raw/gbif/0069607-200613084148143.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Select a sub-sample
gbif_s <- sample_n(gbif, 10000)

# Save file
data.table::fwrite(gbif_s, "Input_files/gbif.csv")



# icmbio ------------------------------------------------------------------
icmbio <-
  vroom(
    "Data_raw/icmbio/portalbio_export_28-09-2020-17-41-57.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Select a sub-sample
icmbio_s <- sample_n(icmbio, 10000)

# Save file
data.table::fwrite(icmbio_s, "Input_files/icmbio.csv")



# idigibio ----------------------------------------------------------------
idigbio_brazil <-
  vroom(
    "Data_raw/idigbio/c7525055-fd26-4c13-9859-dc298d57ff09.zip",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

idigbio_brasil <-
  vroom(
    "Data_raw/idigbio/eee7a8c2-aa07-4a83-b15b-ee02d260e369.zip",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

# Merge datasets
idigbio <- rbind(idigbio_brazil, idigbio_brasil)

idigbio_s <- sample_n(idigbio, 10000)

# Save file
data.table::fwrite(idigbio_s, "Input_files/idigbio.csv")



# neotroptree -------------------------------------------------------------
neotroptree <-
  vroom(
    "Data_raw/neotroptree/species-lat-long_long-table.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

neotroptree_s <- sample_n(neotroptree, 10000)

# Save file
data.table::fwrite(neotroptree_s, "Input_files/neotroptree.csv").csv



# sibbr -------------------------------------------------------------------
txt_files <-list.files (path = "Data_raw/sibbr/unziped_files/",
                        pattern = "*.txt",
                        full.names = T)

sibbr <- as_tibble (rbindlist (lapply (txt_files, fread), fill=TRUE))

sibbr_s <- sample_n(sibbr, 10000)

# Save file
data.table::fwrite(sibbr_s, "Input_files/sibbr.csv")



# specieslink -------------------------------------------------------------
specieslink <-
  vroom(
    "Data_raw/specieslink/speciesLink_all_97889_20200925235952.txt",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

specieslink_s <- sample_n(specieslink, 10000)

# Save file
data.table::fwrite(specieslink_s, "Input_files/specieslink.csv")




# at_trees ----------------------------------------------------------------
at_trees <-
  vroom(
    "Data_raw/at_trees/Zwiener_2017.csv",
    locale = locale(encoding = "UTF-8"), trim_ws = T)

at_trees_s <- sample_n(at_trees, 10000)

# Save file
data.table::fwrite(at_trees_s, "Input_files/at_trees.csv")

