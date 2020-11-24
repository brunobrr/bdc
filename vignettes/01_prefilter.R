# Load all function required
devtools::load_all()

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

fs::dir_create(here::here("Output/Check"))

merged <-
  here::here("data", "temp", "standard_database.xz") %>%
  vroom()

# NOTE: todas as etapas serão aplicadas no merged, sem exclusão
# NOTE: tudo com .flag

# CHECK: 1. Usar a função que o @sjevelazco criou para corrigir xy trocados

wiki_cntr <- bdc_get_wiki_country()

worldmap <- bdc_get_world_map()

standard_country_names <-
  bdc_standard_country(
    data = merged,
    cntry = "country",
    cntry_names_db = wiki_cntr
  )

merged <-
  merged %>%
  left_join(standard_country_names, by = c("country" = "cntr_original"))

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
  pull(database_id)

rows_to_insert <-
  corrected_coordinates %>%
  # remove columns with coordinates transposed
  select(-decimalLatitude, -decimalLongitude) %>%
  # new columns coordinates with the corrected info
  rename(
    decimalLatitude = decimalLatitude_modified,
    decimalLongitude = decimalLongitude_modified
  ) %>%
  # flag all of them
  mutate(.transposed_xy = TRUE)

merged <-
  merged %>%
  # remove wrong coordinates
  filter(!database_id %in% rows_to_remove) %>%
  # flag no issued rows as FALSE
  mutate(.transposed_xy = FALSE) %>%
  # add corrected coordinates
  bind_rows(rows_to_insert)

merged %>% bdc_check_flags()

corrected_coordinates %>%
  select(database_id, scientificName, contains("decimal"), 
         locality, stateProvince, cntr_suggested) %>%
  write_csv(here::here("Output", "Check", "01_prefilter_transposed_coordinates.csv"))

# CHECK: 2. Remover registros com coordenadas inválidas (NA, empty)
# NOTE: no log

merged <-
  merged %>%
  mutate(
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
  mutate(
    .outside_xy = case_when(
      decimalLatitude < -90 | decimalLatitude > 90 ~ TRUE,
      decimalLongitude < -180 | decimalLongitude > 180 ~ TRUE,
      TRUE ~ FALSE
    )
  )

merged %>%
  bdc_check_flags()

merged %>%
  filter(
    .transposed_xy == FALSE,
    .invalid_xy == FALSE,
    .outside_xy == FALSE
  ) %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)

# CHECK: 3. Remover registros sem lat ou sem long

merged <-
  merged %>%
  mutate(
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
  filter(
    .transposed_xy == FALSE,
    .invalid_xy == FALSE,
    .outside_xy == FALSE,
    .no_xy == FALSE
  ) %>%
  bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)

# CHECK: 4. Registros sem nome ou com nome vazio

merged <-
  merged %>%
  mutate(.no_name = if_else(is.na(scientificName), TRUE, FALSE))

merged %>%
  bdc_check_flags()

merged %>%
  filter(
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
    filter(
      .invalid_xy == FALSE,
      .outside_xy == FALSE,
      .transposed_xy == FALSE,
      .no_xy == FALSE,
    ) %>%
    mutate(.in_brazil = cc_sea(
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
    filter(
      .transposed_xy == FALSE,
      .invalid_xy == FALSE,
      .outside_xy == FALSE,
      .no_xy == FALSE,
      .no_name == FALSE,
      .in_brazil == FALSE
    ) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
  filter_for_brazil %>%
    filter(
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
    filter(
      .transposed_xy == FALSE,
      .invalid_xy == FALSE,
      .outside_xy == FALSE,
      .no_xy == FALSE,
    )
  
  selec_points <-
    raster::extract(
      brazil,
      occurrences %>% select(decimalLongitude, decimalLatitude),
      buffer = 2000
    )
  
  filter_for_brazil_by_hand <-
    occurrences %>%
    mutate(.in_brazil = selec_points$fill)
  
  filter_for_brazil_by_hand %>%
    filter(.in_brazil == 1) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
  filter_for_brazil_by_hand %>%
    filter(is.na(.in_brazil)) %>%
    bdc_quickmap(long = decimalLongitude, lat = decimalLatitude)
  
}

# CHECK: 6. Remover registros fósseis ou com origem duvidosa
# FIXME: rever doubt

doubt <- c("Amost", "DrawingOrPhotograph", "Dupli", "EX", "E", "Extra", "F",
           "FOSSIL_SPECIMEN", "FossilSpecimen", "HS", "HUCP",
           "MACHINE_OBSERVATION", "MachineObservation", "MultimediaObject",
           "QQQQQ", "REPET", "RON", "V",  "X", "XS", "O", "S")

merged <-
  merged %>%
  mutate(.doubtful_provenance = if_else(basisOfRecord %in% doubt, TRUE, FALSE))

# CHECK: 7. Salvar tabela de nomes sem coordenadas (x ou y) mas que contém informações sobre localidade

rows_to_insert <-
  merged %>%
  filter(!is.na(scientificName)) %>%
  filter(is.na(decimalLatitude) | is.na(decimalLongitude)) %>%
  filter(!is.na(locality)) %>%
  mutate(.locality_no_xy = TRUE)

rows_to_remove <-
  rows_to_insert %>%
  pull(database_id)

merged <-
  merged %>%
  # remove rows wihout xy but with locality
  filter(!database_id %in% rows_to_remove) %>%
  # flag no issued rows as FALSE
  mutate(.locality_no_xy = FALSE) %>%
  # insert rows flagedd as TRUE
  bind_rows(rows_to_insert)

rows_to_insert %>%
  write_csv(here::here("Output", "Check", "01_prefilter_no_coordinates_but_locality.csv"))

