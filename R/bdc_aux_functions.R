
# Ipak --------------------------------------------------------------------
# Used to install and load multiple R packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}


# bdc_create_dir ----------------------------------------------------------
bdc_create_dir <- function(){
  fs::dir_create(here::here("Output/Check"))
  fs::dir_create(here::here("Output/Intermediate"))
  fs::dir_create(here::here("Output/Report"))
  fs::dir_create(here::here("Output/Figures"))
}

# bdc_get_world_map -------------------------------------------------------
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
    message("Column '.summary' already exist. It will be updated")
    
    data <-
      data %>%
      dplyr::select(-.summary)
    
    df <- 
      data %>%
      dplyr::select(contains(".")) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)
    
    df <- dplyr::bind_cols(data, df) %>% dplyr::select(everything(), .summary)
  } else{
    df <-
      data %>%
      dplyr::select(dplyr::contains(".")) %>%
      dplyr::mutate(.summary = rowSums(.) / ncol(.) == TRUE) %>%
      dplyr::select(.summary)
    
    df <- dplyr::bind_cols(data, df)
  }
  
  message(
    paste(
      "\nbdc_summary_col:\nFlagged",
      sum(df$.summary == FALSE),
      "records.\nOne column was added to the database.\n"
    )
  )
  
  return(df)
}

# bdc_create_report -------------------------------------------------------
bdc_create_report <- function(data, workflow_step) {
  suppressMessages({
    # Total number of records
    if (!file_exists("data/n_records.csv")) {
      n_records <-
        data %>%
        dplyr::summarise(n = n())

      data.table::fwrite(n_records, "data/n_records.csv")

      # Total number of records per database
      n_record_database <-
        data %>%
        dplyr::mutate(
          database_id = gsub("[[:digit:]]+", "", database_id),
          database_id = gsub("_", "", database_id)
        ) %>%
        dplyr::group_by(database_id) %>%
        dplyr::summarise(n_total = n())

      data.table::fwrite(n_record_database, "data/n_record_database.csv")
    } else {
      n_records <- read_csv("data/n_records.csv") %>% dplyr::pull(n)
      n_record_database <- read_csv("data/n_record_database.csv")
    }

    # Prefilter
    if (workflow_step == "prefilter") {
      pf <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t() %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(
          Perc_records_flagged =
            round((V1 / nrow(data) * 100), 2)
        ) %>%
        dplyr::rename(
          Test_name = `NA`,
          Records_flagged = V1
        )

      pf <-
        pf %>%
        dplyr::mutate(Description = Test_name) %>%
        dplyr::mutate(
          Description = if_else(
            Description == ".missing_name",
            "Records missing scientific name",
            Description
          ),
          Description = if_else(Description == ".missing_xy",
            "Records missing coordinates",
            Description
          ),
          Description = if_else(
            Description == ".invalid_xy",
            "Records with invalid coordiantes",
            Description
          ),
          Description = if_else(
            Description == ".xy_provenance",
            "Records from doubtful provenance",
            Description
          ),
          Description = if_else(
            Description == ".xy_out_country",
            "Records outside one or multiple focal countries",
            Description
          ),
          Description = if_else(Description == ".summary",
            "Summary of all tests", Description
          )
        )

      pf <-
        pf %>%
        dplyr::add_row()

      pf <- pf %>% dplyr::select(Description, everything())

      pf[1 + nrow(pf), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )

      names(pf)[4] <- "perc_number_records(*)"
      data <- pf
    }

    # Taxonomy
    if (workflow_step == "taxonomy") {
      names <-
        data %>%
        group_by(notes) %>%
        summarise(number_of_records = n()) %>%
        mutate(
          perc_number_records =
            round((number_of_records / n_records) * 100, 2)
        )

      if (".uncer_terms" %in% names(data)) {
        taxo_unc <-
          data %>%
          dplyr::select(.uncer_terms) %>%
          dplyr::filter(.uncer_terms == FALSE) %>%
          dplyr::group_by(.uncer_terms) %>%
          dplyr::summarise(number_of_records = n()) %>%
          dplyr::mutate(
            perc_number_records =
              round((number_of_records / n_records) * 100, 2)
          ) %>%
          dplyr::rename(notes = .uncer_terms) %>%
          dplyr::mutate(notes = if_else(notes == "FALSE", "taxo_uncer", notes))
      }

      names <- dplyr::bind_rows(names, taxo_unc)

      names <-
        names %>%
        dplyr::mutate(notes = if_else(notes == "", "valid names", notes)) %>%
        dplyr::mutate(Description = notes) %>%
        dplyr::mutate(
          Description = if_else(
            Description == "was misspelled|replaced synonym",
            "valid: was misspelled and replaced synonym",
            Description
          ),
          Description = if_else(
            Description == "taxo_uncer",
            "check: doubtful taxonomic identification",
            Description
          ),
          Description = if_else(Description == "was misspelled",
            "valid: was misspelled", Description
          ),
          Description = if_else(Description == "not found",
            "invalid: not found", Description
          ),
          Description = if_else(Description == "|replaced synonym",
            "valid: replaced synonym", Description
          ),
          Description = if_else(
            Description == "|check +1 accepted",
            "check: more than one accepted name found",
            Description
          ),
          Description = if_else(
            Description == "|check no accepted name",
            "check: no accepted name found",
            Description
          )
        )

      names <-
        names %>%
        dplyr::add_row()

      names <- names %>% dplyr::select(Description, everything())

      names[1 + nrow(names), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )

      names(names)[4] <- "perc_number_records(*)"
      data <- names
    }

    # Space
    if (workflow_step == "space") {
      space <-
        data %>%
        dplyr::select(contains(".")) %>%
        dplyr::summarise_all(., .funs = sum) %>%
        t() %>%
        tibble::as_tibble(rownames = "NA") %>%
        dplyr::mutate(V1 = nrow(data) - V1) %>%
        dplyr::mutate(
          Perc_records_flagged =
            round((V1 / n_records * 100), 2)
        ) %>%
        dplyr::rename(
          Test_name = `NA`,
          Records_flagged = V1
        )

      space <-
        space %>%
        dplyr::mutate(Description = Test_name) %>%
        dplyr::mutate(
          Description = if_else(Description == ".val",
            "Coordinates valid",
            Description
          ),
          Description = if_else(Description == ".equ",
            "Identical coordinates",
            Description
          ),
          Description = if_else(Description == ".zer",
            "Plain zeros",
            Description
          ),
          Description = if_else(Description == ".cap",
            "Records around country capital centroid",
            Description
          ),
          Description = if_else(Description == ".cen",
            "Records around country or province centroids",
            Description
          ),
          Description = if_else(Description == ".otl",
            "Geographical outliers",
            Description
          ),
          Description = if_else(Description == ".gbf",
            "Records around the GBIF headquarters",
            Description
          ),
          Description = if_else(Description == ".inst",
            "Records around biodiversity institutions",
            Description
          ),
          Description = if_else(Description == ".dpl",
            "Duplicated coordinates per species",
            Description
          ),
          Description = if_else(Description == ".rou",
            "Rounded coordinates",
            Description
          ),
          Description = if_else(Description == ".coun",
            "Coordinate-country discordance",
            Description
          ),
          Description = if_else(Description == ".summary",
            "Summary of all tests", Description
          )
        )

      space <-
        space %>%
        dplyr::add_row()

      space <- space %>% dplyr::select(Description, everything())

      space[1 + nrow(space), 1] <-
        paste(
          "(*) calculated in relation to total number of records, i.e.",
          n_records,
          "records"
        )

      names(space)[4] <- "perc_number_records(*)"
      data <- space

    }
  })
    return(data)
}
# bdc_filter_out_flags ----------------------------------------------------
#' Filter out rows based on flags assigned as FALSE
#'
#' @description
#' This functions filter out rows based on any flag column assigned as FALSE
#'
#' @param data data.frame with flags created by functions bdc_flag_*
#' @param logical. Should the column .summary be removed?
#' @importFrom dplyr select filter_at
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_transposed_xy() %>%
#'   bdc_filter_out_flags()
#' }
bdc_filter_out_flags <- function(data, col_to_remove = "all") {
  
  if (col_to_remove %in% "all"){
    column_names <-
      data %>%
      dplyr::select(starts_with(".")) %>%
      names()
    
    data <-
      data %>%
      dplyr::select(-all_of(column_names))
  } else {
    w <- which(names(data) %in% col_to_remove)
    
    column_names <- names(data)[w]
    
    data <-
      data %>%
      dplyr::select(-all_of(column_names))
  }

  message("\nbdc_fiter_out_flags:\nThe following columns were removed from the database:\n", paste(column_names, collapse = ", "))
  
  return(data)
  
}

# bdc_filter_out_names ----------------------------------------------------
bdc_filter_out_names <-
  function(data,
           notes = c("not_found", "more_one_accepted", 
                     "no_accepted", "taxo_uncer"),
           opposite = FALSE) {
    
    data <-
      data %>%
      dplyr::mutate(temp_id = 1:nrow(data))
    
    posi <- NULL
    
    if (any(notes == "not_found")) {
      x <- which(data$notes == "not found")
      posi <- c(posi, x)
    }
    
    if (any(notes == "more_one_accepted")) {
      x <- 
        stringr::str_which(data$notes, regex("1 accepted"))
      posi <- c(posi, x)
    }
    
    if (any(notes == "no_accepted")) {
      x <-
        stringr::str_which(data$notes, regex("check no accepted name"))
      posi <- c(posi, x)
    }
    
    if (any(notes == "taxo_uncer")) {
      x <- which(data$.uncer_terms == FALSE)
      posi <- c(posi, x)
    }
    
    res_temp <- data[posi, ] %>% dplyr::distinct(temp_id, .keep_all = T)
    
    if (opposite == FALSE) {
      res <-
        res_temp %>%
        dplyr::select(-temp_id)
    } else {
      res <-
        data %>%
        dplyr::filter(!temp_id %in% res_temp$temp_id) %>%
        dplyr::select(-temp_id)
    }
    return(res)
  }