# Testing standardize_dataset function ----------------------------------------
if (!require("here")) install.packages("here")

merged_filename <- here::here("data", "temp", "standard_database.xz")

if (!file.exists(merged_filename)) {

  source(here::here("R/aux_functions.R"))

  ipak(
    c(
      "tidyverse",
      "here",
      "glue",
      "fs",
      "janitor",
      "vroom",
      "waldo",
      "tidylog"
    )
  )

  metadata <- readr::read_csv(here::here("Config/DatabaseInfo.csv"))

  standardize_dataset(metadata = metadata)

  # Testing if vroom can concatenate all the resulting standandized databases ---
  merged_database <-
    here::here("data", "temp") %>%
    fs::dir_ls(regexp = "*.xz") %>%
    purrr::map_dfr(
      ~ vroom::vroom(
          file = .x,
          guess_max = 10^6,
          col_types = cols(.default = "c") #,
        )
    )

  merged_database %>%
    mutate(database_name = str_remove(database_id, "_[0-9].*")) %>%
    distinct(database_name)

  waldo::compare(
    x = merged_database %>% names(),
    y = metadata %>% names()
    )

  merged_database %>%
    vroom::vroom_write(merged_filename)

} else {

  message(paste(merged_filename, "already exists!"))

}

# Prefiltering data -----------------------------------------------------------
source(here::here("R/aux_functions.R"))

ipak(
  c(
    "tidyverse",
    "here",
    "fs",
    "vroom",
    "tidylog"
  )
)

fs::dir_create(here::here("output/00_prefilter"))

merged <-
  here::here("data", "temp", "standard_database.xz") %>%
  vroom()

# CHECK: 1. Usar a função que o @sjevelazco criou para corrigir xy trocados

wiki_cntr <- get_wiki_country()

worldmap <- get_world_map()

standard_country_names <-
  standard_country(
    data = merged,
    cntry = "country",
    cntry_names_db = wiki_cntr
  )

merged <-
  merged %>%
  left_join(standard_country_names, by = c("country" = "cntr_original"))

corrected_coordinates <-
  correct_coordinates(
    data = merged,
    x = "decimalLongitude",
    y = "decimalLatitude",
    sp = "scientificName",
    id = "database_id",
    cntr_iso2 = "cntr_iso2c",
    world_poly = worldmap,
    world_poly_iso = "iso2c"
  )

id_remove <-
  corrected_coordinates %>% pull(database_id)

merged <-
  merged %>%
  # remove wrong coordinates
  filter(!database_id %in% id_remove) %>%
  # add corrected coordinates
  bind_rows(corrected_coordinates) %>%
  # flag changed coordinates
  mutate(
    .xy_issue = case_when(
                  is.na(decimalLatitude_modified) | is.na(decimalLongitude_modified) ~ TRUE,
                  TRUE ~ FALSE
                )
  )

# CHECK: 2. Remover registros com coordenadas inválidas
# CHECK: 3. Remover registros sem lat ou sem long

merged %>%
  quickmap()

with_valid_coordinates <-
  merged %>%
  # filter out coordinates outside our world
  filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%
  filter(
    between(decimalLatitude, -90, 90),
    between(decimalLongitude, -180, 180)
   )

with_valid_coordinates %>%
  quickmap()

export_rejected_data(
  raw_data = merged,
  filtered_data = with_valid_coordinates,
  save_in_filename = here::here("output/00_prefilter/invalid_coordinates.csv"),
  comment = "prefilter step reporting data with invalid coordinates"
  )

# CHECK: 4. Registros em nome ou com nome vazio

with_valid_names <-
  with_valid_coordinates %>%
  filter(!is.na(scientificName))

export_rejected_data(
  raw_data = with_valid_coordinates,
  filtered_data = with_valid_names,
  save_in_filename = here::here("output/00_prefilter/invalid_names.csv"),
  comment = "prefilter step reporting data with invalid names"
  )

# CHECK: 5. Registros fora do país selecionado

inside_brazil <-
  with_valid_names %>%
  filter(cntr_iso2c == "BR")

export_rejected_data(
  raw_data = with_valid_names,
  filtered_data = inside_brazil,
  save_in_filename = here::here("output/00_prefilter/outside_brazil.csv"),
  comment = "prefilter step reporting data outside Brazil borders"
  )

# CHECK: 6. Remover registros fósseis ou com origem duvidosa

doubt <- c("Amost", "DrawingOrPhotograph", "Dupli", "EX" , "E", "Extra", "F",
           "FOSSIL_SPECIMEN", "FossilSpecimen", "HS", "HUCP",
           "MACHINE_OBSERVATION", "MachineObservation", "MultimediaObject",
           "QQQQQ", "REPET", "RON", "V",  "X", "XS")

valid_provenance <-
  inside_brazil %>% 
  filter(!basisOfRecord %in% doubt)

export_rejected_data(
  raw_data = inside_brazil,
  filtered_data = valid_provenance,
  save_in_filename = here::here("output/00_prefilter/invalid_provenance.csv"),
  comment = "prefilter step reporting data wit invalid provenance"
  )


# CHECK: 7. Salvar tabela na de nomes sem coordenadas (x ou y) mas que 
# contém informações sobre localidade

with_locality <-
  merged %>%
  filter(!is.na(scientificName)) %>%
  filter(is.na(decimalLatitude) | is.na(decimalLongitude)) %>%
  filter(!is.na(locality))

export_rejected_data(
  raw_data = merged,
  filtered_data = with_locality,
  save_in_filename = here::here("output/00_prefilter/no_coordinates_but_locality.csv"),
  comment = "prefilter step reporting data without coordinates, but with locality info"
  )
