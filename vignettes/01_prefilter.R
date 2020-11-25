
# Install and load packages
ipak(
  c(
    "tidyverse",
    "here",
    "fs",
    "vroom",
    "CoordinateCleaner",
    "rnaturalearth"
  )
)

# Load all functions of BDC workflow
devtools::load_all()

# Create directories for saving the outputs
fs::dir_create(here::here("Output/Check"))
fs::dir_create(here::here("Output/Intermediate"))

# Load the merge database
merged <-
  here::here("data", "temp", "standard_database.xz") %>%
  vroom()


# CHECK 1. Correct latitude and longitude transposed

# load functions
wiki_cntr <- bdc_get_wiki_country() # get country names from Wikipedia
worldmap <- bdc_get_world_map()  # 

# standardize the name of countries
standardize_country_names <-
  bdc_standardize_country(
    data = merged,
    cntry = "country",
    cntry_names_db = wiki_cntr
  )

merged <-
  merged %>%
  dplyr::left_join(standard_country_names, by = c("country" = "cntr_original"))

# Correct latitude and longitude transposed
corrected_coordinates <-
  bdc_correct_coordinates(
    data = merged,
    x = "decimalLongitude",
    y = "decimalLatitude",
    sp = "scientificName",
    id = "database_id",
    cntr_iso2 = "cntr_iso2c",
    world_poly = worldmap,
    world_poly_iso = "iso2c"
  )

rows_to_remove <-
  corrected_coordinates %>%
  dplyr::pull(database_id)

rows_to_insert <-
  corrected_coordinates %>%
  # remove columns with coordinates transposed
  dplyr::select(-decimalLatitude,-decimalLongitude) %>%
  # new columns coordinates with the corrected info
  dplyr::rename(decimalLatitude = decimalLatitude_modified,
                decimalLongitude = decimalLongitude_modified) %>%
  # flag all of them
  dplyr::mutate(.transposed_xy = TRUE)

merged <-
  merged %>%
  # remove wrong coordinates
  dplyr::filter(!database_id %in% rows_to_remove) %>%
  # flag no issued rows as FALSE
  dplyr::mutate(.transposed_xy = FALSE) %>%
  # add corrected coordinates
  dplyr::bind_rows(rows_to_insert)

merged %>% bdc_check_flags()

corrected_coordinates %>%
  dplyr::select(
    database_id,
    scientificName,
    contains("decimal"),
    locality,
    stateProvince,
    cntr_suggested
  ) %>%
  write_csv(here::here("Output", "Check", "01_prefilter_transposed_coordinates.csv"))


# CHECK: 2. Invalid coordinates (i.e empty or NAs)
merged <-
  merged %>%
  dplyr::mutate(
    .invalid_xy = case_when(
      is.na(decimalLatitude) ~ TRUE,
      is.na(decimalLongitude) ~ TRUE,
      # flag empty coordinates
      nzchar(decimalLatitude) == FALSE ~ TRUE,
      nzchar(decimalLongitude) == FALSE ~ TRUE,
      # opposite cases are flagged as FALSE
      TRUE ~ FALSE
    )
  )

merged %>%
  bdc_check_flags()

# TODO: remover coordenadas além dos limites 90, 180

merged <-
  merged %>%
  dplyr::mutate(
    .outside_xy = case_when(
      decimalLatitude < -90 | decimalLatitude > 90 ~ TRUE,
      decimalLongitude < -180 | decimalLongitude > 180 ~ TRUE,
      TRUE ~ FALSE
    )
  )

merged %>%
  bdc_check_flags()

merged %>%
  dplyr::filter(.transposed_xy == FALSE,
                .invalid_xy == FALSE,
                .outside_xy == FALSE) %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)


# CHECK: 3. Records without latitude or longitude

merged <-
  merged %>%
  dplyr::mutate(
    .no_xy = case_when(
      is.na(decimalLatitude) ~ TRUE,
      is.na(decimalLongitude) ~ TRUE,
      # flag empty coordinates
      nzchar(decimalLatitude) == FALSE ~ TRUE,
      nzchar(decimalLongitude) == FALSE ~ TRUE,
      # opposite cases are flagged as FALSE
      TRUE ~ FALSE
    )
  )

merged %>%
  bdc_check_flags()

merged %>%
  dplyr::filter(.transposed_xy == FALSE,
                .invalid_xy == FALSE,
                .outside_xy == FALSE,
                .no_xy == FALSE) %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)


# CHECK: 4. Invalid scientific name (i.e empty or NA)
merged <-
  merged %>%
  dplyr::mutate(.no_name = if_else(is.na(scientificName), TRUE, FALSE))

merged %>%
  bdc_check_flags()

merged %>%
  dplyr::filter(
    .transposed_xy == FALSE,
    .invalid_xy == FALSE,
    .outside_xy == FALSE,
    .no_xy == FALSE,
    .no_name == FALSE
  ) %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)

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

