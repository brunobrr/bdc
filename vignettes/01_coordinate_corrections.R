##%######################################################%##
#                                                          #
####        Automatic correction of coordinates         ####
#                                                          #
##%######################################################%##

source("https://raw.githubusercontent.com/brunobrr/risk_assessment_flora_Brazil_I/master/R/aux_functions.R")

# devtools::install_github("ropensci/rnaturalearth")
# devtools::install_github("ropensci/rnaturalearthhires")

ipak(
  c(
    'StandardizeText',
    'rangeBuilder',
    'stringr',
    'CoordinateCleaner',
    'rnaturalearth',
    'raster',
    'vroom',
    'dplyr',
    'countrycode',
    'ggplot2',
    'rvest',
    'janitor',
    'xml2',
    'rvest',
    'stringi'
  )
)


##%######################################################%##
#                                                          #
####                 Import database                  ####
#                                                          #
##%######################################################%##

temp <- tempfile(fileext = ".xz")

download.file(
  "https://github.com/brunobrr/risk_assessment_flora_Brazil_I/raw/master/data/temp/standard_database.xz",
  destfile = temp
)

# Occurrence database
occ <- vroom::vroom(temp) #Occurrence database
occ


##%######################################################%##
#                                                          #
####           Country names standardization            ####
#                                                          #
##%######################################################%##
# In order to have a more accurate standardization the less the error of the writing of the name of the countries in your column of country name the greater will be the correction and standardization of these names 

# Read countries names database 
wiki_cntr <- vroom::vroom("https://github.com/brunobrr/risk_assessment_flora_Brazil_I/raw/master/data/wiki_country_names.txt")
wiki_cntr # this is a database with country names in different languages constructed on information of wikipedia
# https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(A-C)


# stadard_country function tray to correct and standardize country names and attribute a two character
# country code based on ISO norms
standar_country <-
  standard_country(data = occ, # your occurrence database
                  cntry = 'country', # column names with country names 
                  cntry_names_db = wiki_cntr) # a dataframe with country names in different language 
standar_country
# In the first columns are listed the country names of your database, the second column (cntr_suggested) is the 
# Country names suggested for each country names listed in the column cntr_original. cntr_iso2c is the columns with country ISO code. 
# cntr_iso2c is an important columns because it will be used in the next procedure to correct those occurrences georreferenced erroneously  
# It is important to check if the original different country names were matched with iso2c correctly 
# standar_country %>% View


# Merge the standar_country with occurrence database database 
occ %>% dim
occ <- dplyr::left_join(occ, standar_country, by=c('country'='cntr_original'))
occ %>% dim
occ %>% dplyr::select(country, cntr_suggested, cntr_iso2c)


##%######################################################%##
#                                                          #
####    Prepare country polygon to check occurrences    ####
#                                                          #
##%######################################################%##
# Download and process country polygon with large scale from naturalearth 
worldmap <- rnaturalearth::ne_countries(scale='large') 
worldmap@data

# Add some iso code to some countries polygons 
iso2c <- countrycode::countrycode(unique(worldmap$name_en),
                                  origin = 'country.name.en',
                                  destination = 'iso2c')

iso3c <- countrycode::countrycode(unique(worldmap$name_en),
                                  origin = 'country.name.en',
                                  destination = 'iso3c')

iso <- data.frame(worldmap@data %>% dplyr::select(name_en, starts_with('iso')),
                  iso2c,
                  iso3c)

filt <- !is.na(iso$iso_a2) & is.na(iso$iso2c)
iso$iso2c[filt] <- iso$iso_a2[filt]

filt <- !is.na(iso$iso_a3) & is.na(iso$iso3c)
iso$iso3c[filt] <- iso$iso_a3[filt]

worldmap@data <- iso
is.na(iso) %>% colSums() #number of polygons without isocode
worldmap@data <- worldmap@data %>% dplyr::select(iso2c, iso3c)

rm(list=c('iso', 'iso2c', 'iso3c', 'filt'))
# plot(worldmap)
# plot(worldmap[is.na(worldmap@data$iso2c),], col='red', add=T) #countries without isocode


##%######################################################%##
#                                                          #
####               Coordinate correction                ####
#                                                          #
##%######################################################%##
# the next code will detect occurrences located outside the county were recorded and with 
# several transformation will tray to correct them.

occ_corrected <- correct_coordinates(
  data = occ,
  x = 'decimalLongitude',
  y = 'decimalLatitude',
  sp = 'scientificName',
  id = 'database_id',
  cntr_iso2 = 'cntr_iso2c',
  world_poly = worldmap,
  world_poly_iso = 'iso2c'
)

# Plot corrected occurrences 
require(ggplot2)
ggplot() +
  geom_polygon(data = worldmap,
               aes(x = long, y = lat, group = group),
               fill = "gray70") +
  geom_point(data = occ_corrected,
             aes(x = decimalLongitude, y = decimalLatitude),
             alpha = 0.5) +
  geom_point() +
  geom_point(
    data = occ_corrected,
    aes(x = decimalLongitude_modified, y = decimalLatitude_modified),
    col = 'red',
    alpha = 0.5
  ) +
  coord_equal()


# Save coordinates corrected database 
occ_corrected %>% vroom::vroom_write('coor_corrected.txt')


# PROCEDURE TO ADD THESE CORRECTED RECORDS TO ORIGINAL DATABASE 
# Arrange  two databases based on ID
occ_corrected <-
  occ_corrected %>%
  dplyr::arrange(database_id)

occ <-
  occ %>%
  dplyr::arrange(database_id) #Occurrence database

# Substitute coordinates of records corrected
filt <- which(occ$database_id %in% occ_corrected$database_id)
occ$coord_modified <- FALSE
occ$coord_modified[filt] <- TRUE

# Coordinate substitution
occ[filt, c('decimalLatitude', 'decimalLongitude')] <-
  occ_corrected[, c('decimalLatitude_modified', 'decimalLongitude_modified')]

# See modified coordinates
occ %>% filter(coord_modified)

# Save new occurrences database
vroom::vroom_write(occ, 'occ_fixed.txt')
