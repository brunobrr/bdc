
# Ipak --------------------------------------------------------------------
# Used to install and load multiple R packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

# bdc_get_world_map -------------------------------------------------------
#' Title
#'
#' @export
#'
bdc_get_world_map <- function() {
  
  worldmap <- rnaturalearth::ne_countries(scale='large') 
  
  # Add some iso code to some countries polygons 
  iso2c <- countrycode::countrycode(unique(worldmap$name_en),
                                    origin = 'country.name.en',
                                    destination = 'iso2c')
  
  iso3c <- countrycode::countrycode(unique(worldmap$name_en),
                                    origin = 'country.name.en',
                                    destination = 'iso3c')
  
  iso <- data.frame(worldmap@data %>% 
                    dplyr::select(name_en, starts_with('iso')),
                    iso2c,
                    iso3c)
  
  filt <- !is.na(iso$iso_a2) & is.na(iso$iso2c)
  iso$iso2c[filt] <- iso$iso_a2[filt]
  
  filt <- !is.na(iso$iso_a3) & is.na(iso$iso3c)
  iso$iso3c[filt] <- iso$iso_a3[filt]
  
  worldmap@data <- 
  iso
  is.na(iso) %>% 
  colSums() #number of polygons without isocode
  
  worldmap@data <-
    worldmap@data %>% 
    dplyr::select(iso2c, iso3c)
  
  return(worldmap)
  
}


# bdc_xy_from_locality ----------------------------------------------------
bdc_xy_from_locality <-
  function(data,
           locality = "locality",
           lon = "decimalLongitude",
           lat = "decimalLatitude") {
    df <-
      data %>%
      dplyr::filter(.invalid_xy == FALSE | .missing_xy == FALSE,
                    .data[[locality]] != "" &
                      !is.na(.data[[locality]]))
    
    save <- here::here("Output/Check/01_xy_from_locality.csv")
    df %>%
      data.table::fwrite(save)
    
    message(
      paste(
        "\nbdc_xy_from_locality",
        "\nFound",
        nrow(df),
        "records missing or with invalid xy but with potentially useful information on locality.\n",
        "\nCheck database in:",
        save
      )
    )
    
    return(df)
  }


# bdc_summary_col ---------------------------------------------------------
bdc_summary_col <- function(data) {
  if (any(names(data) == ".summary")) {
    message("Column '.summary' already exist. It will be updated\n")
    
    df <-
      data %>%
      dplyr::select(-.summary) %>% 
      dplyr::select(contains(".")) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)
  } else{
    df <-
      data %>%
      dplyr::select(dplyr::contains(".")) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)
  }
  
  df <- dplyr::bind_cols(data, df)
  
  message(
    paste(
      "\nbdc_summary_col:\nFlagged",
      sum(df$.summary == FALSE),
      "records.\nOne column was added to the database.\n"
    )
  )
  
  return(df)
}


# bdc_tests_summary -------------------------------------------------------
bdc_tests_summary <- function(data, workflow_step) {
  
  # First, create a table of total number of records per database
  if (file_exists("data/n_records.csv")){
    n_records <- 
      data %>%
      dplyr::summarise(n = n())
    data.table::fwrite(n_records, "data/n_records.csv")
  }


  if (workflow_step == "prefilter"){
  suppressWarnings({
    data <-
      data %>%
      dplyr::select(contains(".")) %>%
      dplyr::summarise_all(., .funs = sum) %>%
      t %>%
      tibble::as_tibble(rownames = "NA") %>%
      dplyr::mutate(V1 = nrow(data) - V1) %>%
      dplyr::mutate(Perc_records_flagged = round((V1 / nrow(data) * 100), 2)) %>%
      dplyr::rename(Name = `NA`,
                    Records_flagged = V1)
  })
  }
  return(data)
}

