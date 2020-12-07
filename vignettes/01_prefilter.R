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
    "rvest"
  )
)

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))

# Load the merge database
merged <-
  here::here("data", "temp", "standard_ICMBIO.xz") %>%
  vroom()

# CHECK 1. Correct latitude and longitude transposed

data_pf1 <- merged %>%
  bcd_flag_transposed_xy()

data_pf1 %>% 
  bdc_check_flags()

data_pf1 %>% 
  bdc_filter_out_flags() %>%
  bdc_check_flags()

# CHECK: 2. Invalid coordinates (i.e empty or NAs)

data_pf2 <-
  merged %>%
  bdc_flag_invalid_xy(long = "decimalLongitude", lat = "decimalLatitude")

data_pf2 %>%
  bdc_check_flags()

data_pf2 %>%
  bdc_filter_out_flags() %>%
  bdc_check_flags()

# TODO: remover coordenadas além dos limites 90, 180

data_pf3 <-
  merged %>%
  bdc_flag_outside_xy()

data_pf3 %>%
  bdc_check_flags()

data_pf3 %>%
  bdc_filter_out_flags() %>%
  bdc_check_flags()

data_pf3 %>%
  bdc_filter_out_flags() %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)

# CHECK: 3. Records without latitude or longitude

data_pf4 <-
  merged %>%
  bdc_flag_no_xy()

data_pf4 %>%
  bdc_check_flags()

data_pf4 %>%
  bdc_filter_out_flags() %>%
  bdc_check_flags()

# CHECK: 4. Invalid scientific name (i.e empty or NA)

data_pf5 <-
  merged %>%
  bdc_flag_no_name()

data_pf5 %>%
  bdc_check_flags()

data_pf5 %>%
  bdc_filter_out_flags() %>%
  bdc_check_flags()

# CHECK: 5. Registros fora do país selecionado
# TODO: filtrar pelo polígono
# TODO: testar cc_sea e manualmente: ver issue #32

if (FALSE) {
  
  brazil_polygon <-
    rnaturalearth::ne_countries(country = "brazil", scale = "large", returnclass = "sp")
  
  filter_for_brazil <-
    merged %>%
    dplyr::filter(.invalid_xy == FALSE,
                  .outside_xy == FALSE,
                  .transposed_xy == FALSE,
                  .no_xy == FALSE,) %>%
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
      .transposed_xy == FALSE,
      .invalid_xy == FALSE,
      .outside_xy == FALSE,
      .no_xy == FALSE,
      .no_name == FALSE,
      .in_brazil == FALSE
    ) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
  filter_for_brazil %>%
    dplyr::filter(
      .transposed_xy == FALSE,
      .invalid_xy == FALSE,
      .outside_xy == FALSE,
      .no_xy == FALSE,
      .no_name == FALSE,
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

# CHECK: 6. Invalid provenance
# FIXME: rever doubt

doubt <- c("Amost", "DrawingOrPhotograph", "Dupli", "EX", "E", "Extra", "F",
           "FOSSIL_SPECIMEN", "FossilSpecimen", "HS", "HUCP",
           "MACHINE_OBSERVATION", "MachineObservation", "MultimediaObject",
           "QQQQQ", "REPET", "RON", "V",  "X", "XS", "O", "S")

merged <-
  merged %>%
  dplyr::mutate(.doubtful_provenance = if_else(basisOfRecord %in% doubt, TRUE, FALSE))

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

