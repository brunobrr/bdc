# Load all functions
devtools::load_all()


# FIXEME: add other packages needed to run the function
ipak(
  c(
    "tidyverse",
    "vroom",
    "here",
    "dplyr", 
    "rnaturalearth",
    "CoordinateCleaner",
    "stringr",
    "rworldmap", # Check this
    "geobr", # Check this
    "raster", # Check this
    "flora"
  )
)


# Load the database
data_to_load <- here::here("data", "temp", "standard_database.xz")
df <- vroom(data_to_load)



# Flagging common spatial errors using CoordinateCleaner ------------------

# Use wrapper function "clean_coordinates" for checking several issues present in lat/long coordinates using multiple empirical tests to flag potentially erroneous coordinates. All coordinates must be in WGS84 to use the clean_coordinates function.

continent_border <-
  rnaturalearth::ne_download(scale = "large",
                             type = 'land',
                             category = 'physical')

flag_invalid_xy <- cc_val(x =  df,
                          lon = "decimalLongitude",
                          lat = "decimalLatitude",
                          value = "clean")
space_issues  <-
  clean_coordinates(
    x =  flag_invalid_xy,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = NULL,
    tests = c(
      "capitals",
      "centroids",
      "duplicates",
      "equal",
      "gbif",
      "institutions",
      "outliers",
      "seas", # Check whether this is necessary
      "zeros"
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
    seas_ref = continent_border,
    seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid"
  )

summary(space_issues)
space_issues <- as_tibble(space_issues)

bdc_quickmap(data = space_issues,
             long = decimalLongitude, 
             lat = decimalLatitude)



# Number of records flagged per issue
(!(space_issues %>% dplyr::select(.val:.summary))) %>% colSums()



# FIXEME: STOPPED HEREgt  ########################

#  Flagging low decimal precision -----------------------------------------

round_issue <-
  bdc_round_dec(
    data = df,
    lon = 'longitude',
    lat = 'latitude',
    ndec = c(0, 1, 2) #number of decimals to be tested
  )
colSums(!round_issue)

space_issues <-
  dplyr::bind_cols(
    space_issues %>% dplyr::select(-.summary),
    round_issue,
    space_issues %>% dplyr::select(.summary)
  )

rm(round_issue)

# update .summary column
df <- df %>% rowwise() %>% mutate(.summary=all(.ndec_all, .summary))
df %>% select(starts_with('.'))





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
