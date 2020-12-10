# Load all functions of BDC workflow
devtools::load_all()

# Install and load packages
ipak(
  c(
    "tidyverse",
    "here",
    "fs",
    "vroom",
    "CoordinateCleaner",
    "rnaturalearth",
    "dplyr",
    "xml2",
    "rvest",
    "qs", 
    "sf", 
    "rnaturalearth"
  )
)

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))

# Load the merge database
merged <-
  here::here("data", "temp", "standard_database.qs") %>%
  qs::qread()

# CHECK 1: Flag records missing scientific name (i.e empty or NA)
data_pf1 <-
  merged %>%
  mutate(.missing_name =
           bdc_flag_missing_names(data = ., sci_name = "scientificName"))

# CKECK 2: Flag records missing latitude or longitude 
data_pf2 <-
  data_pf1 %>%
  mutate(.missing_xy =
           bdc_flag_missing_xy(., lon = "decimalLongitude", 
                               lat = "decimalLatitude"))

# CHECK 3: Flag records with invalid coordinates
# data_pf3 <-
#   data_pf2 %>%
#   mutate(.invalid_xy =
#            bdc_flag_invalid_xy(., lon = "decimalLongitude", 
#                                lat = "decimalLatitude"))
data_pf3 <-
  data_pf2 %>%
  mutate(.invalid_xy =
           CoordinateCleaner::cc_val(x = ., 
                                     lon = "decimalLongitude", 
                                     lat = "decimalLongitude", 
                                     value = "flag"))
                                      
# CKECK 4: Flag records from doubtful provenance
data_pf4 <-
  data_pf3 %>%
  mutate(.xy_provenance =
           bdc_flag_xy_provenance(., basisOfRecord = "basisOfRecord"))





# CHECK 1. Correct latitude and longitude transposed

data_pf1 <- merged %>%
  bcd_flag_transposed_xy()

data_pf1 %>% 
  bdc_check_flags()

data_pf1 %>% 
  bdc_filter_out_flags() %>%
  bdc_check_flags()






# CHECK: 5. Registros fora do país selecionado
# TODO: filtrar pelo polígono
# TODO: testar cc_sea e manualmente: ver issue #32

if (FALSE) {
  
  brazil_polygon <-
    rnaturalearth::ne_countries(country = "brazil",
                                scale = "large",
                                returnclass = "sp")
  
  
  
  # continent_border <-
  #   rnaturalearth::ne_download(scale = "large",
  #                              type = 'land',
  #                              category = 'physical')
  
  teste <- cc_val(x = merged, lon = "decimalLongitude", "decimalLatitude")
  
  filter_for_brazil <-
    # merged %>%
    # dplyr::filter(.invalid_xy == FALSE,
    #               .outside_xy == FALSE,
    #               .transposed_xy == FALSE,
    #               .no_xy == FALSE,) %>%
    teste %>% 
    dplyr::mutate(
      .in_brazil = cc_sea(
        x = .,
        lon = "decimalLongitude",
        lat = "decimalLatitude",
        ref = brazil_polygon,
        value = "flagged"
      )
    )
  
  filter_for_brazil %>%
    bdc_check_flags()
  
  filter_for_brazil %>%
    dplyr::filter(
      # .transposed_xy == FALSE,
      # .invalid_xy == FALSE,
      # .outside_xy == FALSE,
      # .no_xy == FALSE,
      # .no_name == FALSE,
      .in_brazil == FALSE
    ) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
  filter_for_brazil %>%
    dplyr::filter(
      # .transposed_xy == FALSE,
      # .invalid_xy == FALSE,
      # .outside_xy == FALSE,
      # .no_xy == FALSE,
      # .no_name == FALSE,
      .in_brazil == TRUE
    ) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
}

# NOTE: WIP do not run
if (FALSE) {
  
  world_polygon <- rworldmap::getMap()
  
  brazil <- world_polygon[which(world_polygon$NAME == "Brazil"), ]
  
  brazil@data <- brazil@data %>% dplyr::select(ADMIN)
  
  brazil@data$fill <- 1
  
  occurrences <-
    merged %>%
    dplyr::filter(.transposed_xy == FALSE,
                  .invalid_xy == FALSE,
                  .outside_xy == FALSE,
                  .no_xy == FALSE,)
  
  selec_points <-
    raster::extract(
      brazil,
      occurrences %>% select(decimalLongitude, decimalLatitude),
      buffer = 2000
    )
  
  filter_for_brazil_by_hand <-
    occurrences %>%
    dplyr::mutate(.in_brazil = selec_points$fill)
  
  filter_for_brazil_by_hand %>%
    filter(.in_brazil == 1) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
  filter_for_brazil_by_hand %>%
    filter(is.na(.in_brazil)) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
}

(.doubtful_provenance = if_else(basisOfRecord %in% doubt, TRUE, FALSE))

# CHECK: 7. Salvar tabela de nomes sem coordenadas (x ou y) mas que contém informações sobre localidade

rows_to_insert <-
  merged %>%
  dplyr::filter(!is.na(scientificName)) %>%
  dplyr::filter(is.na(decimalLatitude) |
                  is.na(decimalLongitude)) %>%
  dplyr::filter(!is.na(locality)) %>%
  dplyr::mutate(.locality_no_xy = TRUE)

rows_to_remove <-
  rows_to_insert %>%
  dplyr::pull(database_id)

merged <-
  merged %>%
  # remove rows without xy but with locality
  dplyr::filter(!database_id %in% rows_to_remove) %>%
  # flag no issued rows as FALSE
  dplyr::mutate(.locality_no_xy = FALSE) %>%
  # insert rows flagged as TRUE
  dplyr::bind_rows(rows_to_insert)

rows_to_insert %>%
  write_csv(here::here("Output", "Check", "01_prefilter_no_coordinates_but_locality.csv"))

