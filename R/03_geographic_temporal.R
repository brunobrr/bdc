##%######################################################%##
#                                                          #
####       Geographical and temporal corrections        ####
#                                                          #
##%######################################################%##

# TRANSFER THIS FUNCTION INTO auxiliary FUNCTIONS
# Function to test round coordinates
bdc_round_dec <- function(data, lon = "decimalLongitude", lat = "decimalLatitude", ndec=c(0,1,2)){
  # data: data.frame. A data.frame with coordinates data
  # lon: character. Column names with longitude values
  # lat: character. Column names with latitude values
  # ndec: numeric. A vector with number of decimal to be tested. Default ndec=c(0,1,2) 
  data <- data[, c(lon, lat)] %>% as.data.frame()
  ndec_lat <- (data[, lat] %>%
                 as.character %>% stringr::str_split_fixed(., pattern = '[.]', n = 2))[, 2] %>%
    stringr::str_length()
  ndec_lon <- (data[, lon] %>%
                 as.character %>% stringr::str_split_fixed(., pattern = '[.]', n = 2))[, 2] %>%
    stringr::str_length()
  rm(data)
  ndec_list <- as.list(ndec)
  names(ndec_list) <- paste0('.', 'ndec', ndec)
  for (i in 1:length(ndec)) {
    message('Testing coordinate with ', ndec[i],' decimal')
    ndec_list[[i]] <- !(ndec_lat == ndec[i] & ndec_lon == ndec[i])
    message('Flagged ', sum(!ndec_list[[i]]), ' records')
  }
  ndec_list <- dplyr::bind_cols(ndec_list)
  ndec_list$.ndec_all <- apply(ndec_list, 1, all) #all flagged as low decimal precision 
  return(ndec_list)
}


bdc_parse_date <- function(data_frame, column_to_test){
  col <- data_frame[[column_to_test]]
  .year <- stringr::str_extract(col, "[[:digit:]]{4}") %>% as.integer()
  .year_val <- dplyr::if_else(.year %in% 1500:year(Sys.Date()), 
                              TRUE, FALSE)
  res <- cbind(data_frame, .year_val, .year)
  return(res)
}

bdc_parse_date <- function(data_frame, column_to_test, old = NULL) {
  col <- data_frame[[column_to_test]]
  year_corrected <- stringr::str_extract(col, "[[:digit:]]{4}") %>% as.numeric()
  
  if(is.null(old)){
    .year_val <-
      dplyr::if_else(year_corrected %in% 1500:lubridate::year(Sys.Date()),
                     TRUE,
                     FALSE)
  } else if(is.numeric(old)){
    .year_val <-
      dplyr::if_else(year_corrected %in% 1500:lubridate::year(Sys.Date()),
                     TRUE,
                     FALSE)
    .year_val <- .year_val & year_corrected>old 
  } else {
    
    stop("The 'old' argument should be used with one year as a numeric data")
    
  }
  
  res <- cbind(data_frame, .year_val, year_corrected)
  return(res)
}

####      Load package and functions       ####
source("https://raw.githubusercontent.com/brunobrr/risk_assessment_flora_Brazil_I/master/R/aux_functions.R")

ipak(
  c(
    "tidyverse",
    "data.table",
    "skimr",
    "naniar",
    "devtools",
    "stringr",
    "tm",
    "vroom",
    "furrr",
    "parallel",
    "doParallel",
    "tidylog",
    "colorspace",
    "taxize",
    "qdap",
    "DT",
    "flora",
    "CoordinateCleaner"
  )
)

####                 Import database                  ####
temp <- tempfile(fileext = ".xz")

download.file(
  "https://github.com/brunobrr/risk_assessment_flora_Brazil_I/raw/master/data/temp/standard_database.xz",
  destfile = temp
)

data_03 <- vroom::vroom(temp) #Occurrence database
names(data_03)

data_03 <-
  data_03 %>%
  # filter coordinates impossible to test or without species
  dplyr::filter(!is.na(scientificName) | !is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%
  dplyr::filter(dplyr::between(decimalLatitude, -90, 90),
                dplyr::between(decimalLongitude, -180, 180))


##%######################################################%##
#                                                          #
####              Flagging common spatial               ####
####           errors using CoordinateCleaner           ####
#                                                          #
##%######################################################%##

# Use wrapper function "clean_coordinates" for checking several issues present in lat/long coordinates using multiple empirical tests to flag potentially erroneous coordinates. 
# All coordinates must be in WGS84 to use the clean_coordinates function.
continent_border <- rnaturalearth::ne_download(scale = "large", type = 'land', category = 'physical')

data_03 <- clean_coordinates(
  x =  data_03,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  countries = NULL,
  tests = c("capitals", "centroids", "duplicates", 
            "equal", "gbif", "institutions", "outliers",
            "seas", "zeros"),
  capitals_rad = 10000,
  centroids_rad = 1000,
  centroids_detail = "both",
  inst_rad = 100,
  outliers_method = "quantile",
  outliers_mtp = 5,
  outliers_td = 1000,
  outliers_size = 7, #acho que esse aqui podemos aumentar visando que existem muitas especeis pouco amostradas 
  range_rad = 0,
  zeros_rad = 0.5,
  capitals_ref = NULL,
  centroids_ref = NULL,
  country_ref = NULL,
  country_refcol = "iso_a3",
  inst_ref = NULL,
  range_ref = NULL,
  seas_ref = continent_border,
  seas_scale = 110,
  urban_ref = NULL,
  value = "spatialvalid"
)

summary(data_03)
data_03 <- as_tibble(data_03)
data_03 %>% filter(!.summary) %>% quickmap()


##%######################################################%##
#                                                          #
####          Flagging low decimal precision            ####
#                                                          #
##%######################################################%##

round_issue <-
  bdc_round_dec(
    data = data_03,
    lon = 'decimalLongitude',
    lat = 'decimalLatitude',
    ndec = c(0, 1, 2) #number of decimals to be tested
  )
colSums(!round_issue)

data_03 <- dplyr::bind_cols(data_03 %>% dplyr::select(-.summary), 
                  round_issue,
                  data_03 %>% dplyr::select(.summary))
rm(round_issue)

# update .summary column
data_03 <- data_03 %>% mutate(.summary=.ndec_all & .summary)
data_03 %>% select(starts_with('.'))


##%######################################################%##
#                                                          #
####         Standardize temporal information           ####
#                                                          #
##%######################################################%##

# Flag incorrect date information (e.g. 0, 2021, NA, 1)
data_03 <- data_03 %>% dplyr::mutate(eventDate=as.integer(eventDate)) 
data_03 <- parse_date(data_frame = data_03, column_to_test = "eventDate")
table(data_03$.year)
table(data_03$.year_val)
data_03
# update .summary column (VER se deixamos esta atualiza??o do .summary)
# data_03 <- data_03 %>% mutate(.summary=.year_val & .summary)

# Save the table
fwrite(data_03, "data/clean/04_database_geographic_temporal_checking/data_03.csv")


# Add species information from the Brazilian Flora Group database ---------

# This database contains information on many group (angio, gymno, bryoph, lyco) # For some reason, get.taxa did not return the establishment of some plant. So we used BFG 2018 database for searching species establishment
library(readr)
data_03 <- vroom("C:/Users/santi/Downloads/Occ_Flora_Brasil/data_04.csv")
set.seed(0)
data_03 <- data_03%>% sample_n(1000)
data_03 <- dplyr::left_join(data_03, bfg_2018[,c('.id', 'scientificName')], by=c('.scientific.name'='scientificName'), ) 

bfg_2018 <-
  read_delim(
    "data/raw/Flora_do_Brasil/BGF-2018.txt",
    "\t",
    escape_double = FALSE,
    col_types = cols(ID = col_number()),
    trim_ws = TRUE
  )
colnames(bfg_2018)[1] <- ".id"

data_03 <- left_join(data_03, bfg_2018, by = ".id")

# Merge information
# Establishment (cultivated, native or naturalized)
sum(is.na(data_03$.establishment)) # 1499626 missing information (data_03)
w <- which(is.na(data_03$.establishment))
data_03$.establishment[w] <- data_03$Origem[w]
sum(is.na(data_03$.establishment)) # now, 1259249 missing information

# Occurence (states SP, MG, ES, etc)
sum(is.na(data_03$.occurrence)) # 1499626 missing information (data_03)
w <- which(is.na(data_03$.occurrence))
data_03$.occurrence[w] <- data_03$Occurrence[w]
sum(is.na(data_03$.occurrence)) # now, 1500263 missing information

# Standardized occurrence names
data_03$.occurrence <- gsub("BR-", "", data_03$.occurrence)
data_03$.occurrence <- gsub("\\|", "; ", data_03$.occurrence)
data_03$Endemic <- gsub("desconhecido", "unknown", data_03$Endemic)

# Remove columns
data_03 <- data_03 %>% 
  dplyr::select(-c(Genus, Species, Subspecies, Variety,   
                   Form, Origem, Occurrence, Rank, Status,
                   'Occur in Brazil'))





##%######################################################%##
#                                                          #
####                Filter species range                ####
#                                                          #
##%######################################################%##

# Check whether the species records occurs in the known species distribution (i.e., Brazilian state). Species distribution information were obtained from Brazilian Flora group (2018)

## Download data from gadm.org 
bra_states <- raster::getData("GADM", country= "Brazil", level=1)

bra_states@data <- bra_states@data %>% dplyr::select(HASC_1)
bra_states@data$fill <- 1
bra_states@data$HASC_1 <- gsub("BR.", "", bra_states@data$HASC_1)
selec_points <- raster::extract(bra_states, data_03 %>% dplyr::select(longitude, latitude))

occ <- data_03$.occurrence
fill <- selec_points$fill
uf <- selec_points$HASC_1

# Function for checking whether the records in inside species range
range <-NULL
for (i in 1:length(occ)){
  range[i] <-  stringr::str_detect(occ[i], stringr::regex(uf[i], ignore_case = TRUE))
}
range <- ifelse(is.na(range), FALSE, range)

# Add the column
data_03$.recordState <- uf
data_03$.range <- range
table(data_03$.range)

# Save the table
fwrite(data_03, "data/clean/04_database_geographic_temporal_checking/data_03_1.csv")





# Create a "clean" fitness-for-use database -------------------------------
data_04 <- vroom(
  "data/clean/04_database_geographic_temporal_checking/data_03_1.csv",
  delim = ",",
  locale = locale(encoding = "UTF-8")
)

# Pallete
pal <- viridisLite::viridis(10)


# Exclude the column ".sea" and uptade the values of the summary column
# We removed test of coords in the sea because points in the sea were previously removed. The remaining points are located close of the coastline (e.g., mangroves)
data_04 <- data_04 %>% dplyr::select(-.sea) 
new.summary <- rowSums(data_04[,23:30])
new.summary <- ifelse(new.summary == 8, TRUE, FALSE)
data_04$.summary <- new.summary

# Figure: Amount of records flagged in each issue
tab <- data_04  %>%
  dplyr::select(.zer:.dpl, -.gbf) %>% 
  colSums() 

tab <- (tab - nrow(data_04)) * -1 # Count records assigned as FALSE
# Proportion of each issue compared with the total of valid records
tab <- tab/nrow(data_04) * 100 
tab <- round(tab, 2)

names(tab) <- c("Zero", "Capital", "Centroid", "Urban",
                "Institution", "Duplicated")
tab <- data.frame(names(tab), tab)
colnames(tab) <- c("Issues", "N")

# Remove record without complete identification
table(data_04$.taxon.rank)

data_04 <- data_04 %>% 
  filter(.taxon.rank %in% c("species", "subspecies", "variety")) #(n = 1188007)

# Remove records not matched with names in the Brazilian oficial list
data_04 <- data_04 %>% drop_na(.scientific.name) # 1188007 removed

# Remove Algae
table(data_04$Group)
# Algas = 45588 
# Angiospermas = 9448937
# Bri\xf3fitas = 168765  
# Gimnospermas  = 6804
# Samambaias e Lic\xf3fitas  = 360295 

data_04 <- data_04 %>% filter(!Group %in% "Algas") # 45588

# Remove records with taxonomic uncertain
data_04 <- data_04 %>% 
  filter(.taxonomic_uncertainty == TRUE) # 23937 removed




# Save a temporary table used for comparising how each geographic issues cause changes in EOO and species' conservation category.

# Save the table (10327163 | 41)
fwrite(data_04, "data/clean/05_clean_databases/data_04_temp.csv")




# Remove problematic coordinates (n = 5625898, most duplicated records, out 10327163, 54%)
# Most problematic records are duplicated ones, followed by records in centroids
data_04 <- data_04 %>% filter(.summary == TRUE) 


# Remove records outside species distribution
# Records missing distribution data were not removed.
table(data_04$.range) # 344154
sum(is.na(data_04$.range)) # 164840
data_04 <- data_04 %>% filter(.range == TRUE | is.na(.range)) # 344154 removed


# Remove columns
data_04 <- data_04 %>% dplyr::select(-c(.val, .zer, .cap, .cen, .urb, .gbf, 
                                        .inst, .dpl, .summary, .range, 
                                        .term_uncertainty, .taxonomic_uncertainty,
                                        .idUnique, .accepted.name, .search.str,
                                        .recordState))

# Standardized column names
colnames(data_04)[20:24] <- c(".group", ".endemic", ".vouchers", 
                              ".reference", ".environment")

colnames(data_04)[8] <- ".id.Flora2020"


# Merge information from raw databases
# 21732211 records | 24 original columns
data_raw <-
  vroom(
    "data/clean/01_merge_all_datasets/merge_all_datasets.csv",
    delim = ",",
    locale = locale(encoding = "UTF-8"))

d <- data_raw %>% dplyr::select(record_id, verified_species_binomial, country, state_province, county, locality, coordinatePrecision, coordinateUncertaintyInMeters, identified_by, recorded_by)

data_04 <- left_join(data_04, d, by = "record_id")

data_04 <-
  data_04 %>% dplyr::select(
    record_id, database_source, verbatim.scientific.name, .original.search,
    .scientific.name:.taxon.status, .group, .notes, latitude, longitude,
    country, state_province, county, locality, year, .year, .threat.status,
    .occurrence, .establishment, .endemic:.environment, coordinatePrecision, 
    coordinateUncertaintyInMeters, identified_by, recorded_by, basisOfRecord,  
    .year_val)


# 2286599 records missing year information (2022510 | 40)
data_05 <- data_04 %>% filter(.year_val == TRUE)

data_04 <- data_04 %>% dplyr::select(-.year_val)
data_05 <- data_05 %>% dplyr::select(-.year_val)


# Total number of species remaining per database
length(unique(data_04$.scientific.name)) # 35679
length(unique(data_05$.scientific.name)) # 33748

# Number of valid records per database
data_04 %>% group_by(database_source) %>% summarise(n = n())
# database_source       n
#   1 ATLANTIC          37491
# 2 BIEN             855843
# 3 DRYFLOR          165046
# 4 GBIF            1176772
# 5 ICMBIO           118657
# 6 IDIGBIO          775889
# 7 NEOTROPTREE      595357
# 8 SIBBR            398552
# 9 SPECIESLINK      103209
# 10 ZWIENER          130295

data_05 %>% group_by(database_source) %>% summarise(n = n())
# database_source       n
#   1 ATLANTIC          19627
# 2 BIEN             166408
# 3 GBIF            1141942
# 4 ICMBIO           117296
# 5 IDIGBIO          179031
# 6 SIBBR            355454
# 7 SPECIESLINK       90754

# Figure: Number of records per species
n_spp <- data_04  %>%  
  group_by(.scientific.name) %>% 
  summarise(total = n())

fwrite(n_spp, "output/results/data_04_NrecordsSpecies.csv")

# Save the tables
fwrite(data_04, "data/clean/05_clean_databases/data_04.csv") # (4357111 | 30)
fwrite(data_05, "data/clean/05_clean_databases/data_05.csv") # (2070512 | 30)