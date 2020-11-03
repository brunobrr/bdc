##%######################################################%##
#                                                          #
####             Standardize country names              ####
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
    'janitor'
  )
)


##%######################################################%##
#                                                          #
####       Countries names in different language        ####
#                                                          #
##%######################################################%##

# Sourced from wikipedia

URL <-
  c(
    "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(A-C)",
    "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(D-I)",
    "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(J-P)",
    "https://en.wikipedia.org/wiki/List_of_country_names_in_various_languages_(Q-Z)"
  )

wiki_cntr <- list()

for(i in 1:length(URL)) {
  
  temp <- URL[i] %>%
    xml2::read_html() %>%
    rvest::html_nodes("table")
  
  temp <- rvest::html_table(temp[2]) ## Just the "legend" table
  
  temp <-
    temp[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!X1 %in% LETTERS, !X1 == 'English name')
  
  base::colnames(temp) <- c('english_name', 'names_in_different_languages')
  wiki_cntr[[i]] <- temp
  
  # wiki_cntr[[i]] <- janitor::clean_names(temp)
  temp <- 
    lapply(wiki_cntr[[i]][, 2] %>%
             pull(1), extract_cntr_names)
  
  names(temp) <- 
    wiki_cntr[[i]] %>%
    dplyr::pull(english_name)
  
  temp <- as_tibble(plyr::ldply(temp))
  wiki_cntr[[i]] <- temp
  colnames(wiki_cntr[[i]])  <- c('english_name', 'names_in_different_languages')
  rm(list = c('temp'))
}

wiki_cntr <- bind_rows(wiki_cntr) %>% dplyr::arrange(english_name)

wiki_cntr$english_name %>% unique
# vroom_write(wiki_cntr, "wiki_country_names.txt")

#Select your country to get its names in different leguages 
wiki_cntr_name <- wiki_cntr %>% dplyr::filter(english_name=='Brazil') %>% pull(2)




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

occ <- vroom::vroom(temp) #Occurrence database

# Create a country database based on occ database
cntr_db <- 
  occ %>% 
  dplyr::distinct(country, .keep_all = FALSE) %>% 
  dplyr::arrange(country) %>% 
  rename(cntr_original = country)

cntr_db$cntr_original2 <-
  stringr::str_replace_all(cntr_db$cntr_original, "[[:punct:]]", " ") %>%
  str_trim()

cntr_db$cntr_suggested <-
  standardizeCountry(cntr_db$cntr_original2,
                     fuzzyDist = 1,
                     nthreads = 1)
cntr_db

# Assign country names based on different character matching.
# This procedure is focused on the target country (Brazil). 
# You will need change it base on your nation of interest

filt <- stringr::str_which(cntr_db$cntr_original,
                     stringr::str_flatten(wiki_cntr_name, collapse = "|"))

# Assign here your country name in English and in upper case (equal to standardizeCountry() function output)

cntr_db$cntr_suggested[filt] <- 'BRAZIL' 

# Warning: this is only for Brazil
filt <- stringr::str_which(cntr_db$cntr_original, "Bra|Br?") 
cntr_db$cntr_suggested[filt] <- 'BRAZIL'

# Country code based on iso2c (it is possible use another code like iso3c, see ?codelist)
cntr_db$cntr_iso2c <-
  countrycode(cntr_db$cntr_suggested,
              origin = 'country.name.en',
              destination = 'iso2c')

# Filter cntr_db for your interest country
cntr_db_brazil <- cntr_db %>%
  dplyr::select(cntr_original, cntr_iso2c) %>%
  dplyr::filter(cntr_iso2c == 'BR') #Here it is used BR because it is the iso2c code for Brazil

cntr_db_brazil # check this if the original different country names were matched with iso2c correctly 
# cntr_db_brazil %>% View

# Joint tow databases to add cntr_iso2c to occ database
occ %>% dim
occ <-
  left_join(occ, cntr_db_brazil, by = c('country' = 'cntr_original'))
occ %>% dim

# Filter database only for those occurrences with cntr_iso2c equal to 'BR' this new occ database will be used to try rescue  occurrences georeferenced outside the target country
occ <- occ %>% dplyr::filter(cntr_iso2c=='BR') #Here it is used BR because it is the iso2c code for Brazil, substitue BR by your iso2c country code. 

rm(list = c('cntr_db_brazil', 'cntr_db', 'filt'))

##%######################################################%##
#                                                          #
####            Test coord change function              ####
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

# plot(worldmap)
# plot(worldmap[is.na(worldmap@data$iso2c),], col='red', add=T) #countries without isocode


# Filter occ database to evoid clean_coordiantes errors
occ2 <-
  occ %>% 
  filter(!is.na(decimalLongitude) |
                   !is.na(decimalLatitude)) %>%
  filter(
    decimalLongitude >= -180,
    decimalLongitude <= 180,
    decimalLatitude >= -90,
    decimalLatitude <= 90
  )

# Detect those record georeferenced outside a country
occ2 <- clean_coordinates(
  x =  occ2,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  countries = "cntr_iso2c", # iso2 code column of our database
  tests = c("seas", "countries"), #Will be tested records located in the see and outside georeferenced countries
    country_ref = worldmap, #Here we are using a high resolution countries border database 
  country_refcol = "iso2c", #iso2 code column of country polygon database
  seas_ref = worldmap, #Here we are using a high resolution countries border database 
  value = "spatialvalid"
)

summary(occ2)

# Separate those records outside study area and have a iso2c code
occ2 <- occ2 %>% 
  as_tibble() %>% 
  dplyr::filter(!.summary, !is.na(cntr_iso2c))

# coord_trans() function will try different coordinate transformations to correct georeferenced occurrences  
coord_test <-
  coord_trans(
    data = occ2,
    x = 'decimalLongitude',
    y = 'decimalLatitude',
    country_code = 'cntr_iso2c',
    id = 'database_id',
    worldmap = worldmap,
    worldmap_cntr_code = 'iso2c'
  )

# nrow(coord_test)/nrow(occ2)*100
rm(occ2)
coord_test %>% dim

# Elimination of those records with more than two proposed correction
coord_test <- coord_test %>% dplyr::distinct(.,database_id, .keep_all=T) 
coord_test %>% dim


# Elimination of those records near to country border (to avoid flip coordinates or sign that fall too close to country border)
my_country <- worldmap[which(worldmap$iso2c == 'BR'), ] #Here filter polygon based on your country iso2c code

my_country2 <-
  raster::buffer(my_country, width = 0.5) #0.5 degree ~50km near to equator
plot(my_country)
plot(my_country2, add = T)

coord_sp <- sp::SpatialPoints(coord_test %>%
                                dplyr::select(decimalLongitude,
                                              decimalLatitude))

coord_sp@proj4string <- my_country2@proj4string
over_occ <- sp::over(coord_sp, my_country2)

coord_test %>%
  dplyr::filter(over_occ == 1) %>%
  dplyr::select(decimalLongitude, decimalLatitude) %>%
  points(., pch = 19, col = 'red')

# Eliminate as corrected those records too close to country border
coord_test <- coord_test %>% dplyr::filter(is.na(over_occ))

# Plot corrected occurrences 
ggplot() +
  geom_polygon(data = my_country,
               aes(x = long, y = lat, group = group),
               fill = "gray70") +
  geom_point(data = coord_test,
             aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point() +
  geom_point(
    data = coord_test,
    aes(x = decimalLongitude_modified, y = decimalLatitude_modified),
    col = 'red'
  ) +
  coord_equal()


# PROCEDURE TO ADD THESE CORRECTED RECORDS TO ORIGINAL DATABASE 
# Arrange  two databases based on ID
coord_test <-
  coord_test %>%
  dplyr::arrange(database_id)

# occ <- occ %>% dplyr::arrange(database_id)
occ <-
  vroom::vroom(temp) %>%
  dplyr::arrange(database_id) #Occurrence database

# Substitute coordinates of records corrected
filt <- which(occ$database_id %in% coord_test$database_id)
occ$coord_modified <- FALSE
occ$coord_modified[filt] <- TRUE

# Coordinate subtitution
occ[filt, c("decimalLatitude", "decimalLongitude")] <-
  coord_test[c("decimalLatitude_modified", "decimalLongitude_modified")]

# See modified coordinates
occ %>% filter(coord_modified)
# vroom::vroom_write(occ, 'occ_some_cord_fixed.txt')
