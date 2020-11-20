
############################################################
#                                                          #
#                           ipak                           #
#                                                          #
############################################################

# usefull to install and load multiple R packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}




############################################################
#                                                          #
#                    00_MERGE_DATASETS                     #
#                                                          #
############################################################

#' Title: Standardize datasets columns based on metadata file
#' 
#' @param metadata a table containing information about which columns of the
#'   original dataset need to be renamed following Darwin Core terminology.
#'   Please see the `Config/DatabaseInfo.csv` file.
#'
#' @importFrom dplyr pull filter select select_if mutate n everything
#' @importFrom fs dir_exists dir_create
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom janitor clean_names make_clean_names
#' @importFrom purrr set_names
#' @importFrom readr read_csv write_csv
#'
#' @export
standardize_dataset <- function(metadata) {
  
  save_in_dir <- here::here("data", "temp")
  
  if (!fs::dir_exists(save_in_dir)) {
    fs::dir_create(save_in_dir)
  }
  
  input_file <-
    metadata %>%
    dplyr::pull(File_name_to_load)
  
  for (file_index in seq_along(input_file)) {
    
    input_filename <-
      metadata %>%
      dplyr::filter(File_name_to_load == input_file[file_index]) %>%
      pull(File_name_to_load)
    
    dataset_name <-
      metadata %>%
      dplyr::filter(File_name_to_load == input_file[file_index]) %>%
      dplyr::select(datasetName) %>%
      dplyr::pull()
    
    save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".xz")
    
    if (!file.exists(save_in_filename)) {
      
      basename_names <-
        metadata %>%
        dplyr::filter(File_name_to_load == input_file[file_index]) %>%
        dplyr::select_if(~ !is.na(.)) %>%
        dplyr::select(-datasetName, -File_name_to_load)
      
      standard_names <-
        basename_names %>%
        names(.)
      
      vector_for_recode <-
        basename_names %>%
        purrr::set_names(standard_names) %>%
        { c(.) } %>%
        unlist()
      
      imported_raw_dataset <-
        here::here(input_file[file_index]) %>%
        vroom::vroom(guess_max = 10^6, col_types = cols(.default = "c"), n_max = 1)
      
      skip_to_next <- FALSE
      
      error_message <-
        paste("[ERROR]: Column names defined in the metadata do not match column names in the", input_filename)
      
      tryCatch(
        
        if (sum(!vector_for_recode %in% names(imported_raw_dataset)) != 0) {
          
          stop(error_message)
          
        } else {
          
          standard_dataset <-
            here::here(input_file[file_index]) %>%
            vroom::vroom(guess_max = 10^6, col_types = cols(.default = "c")) %>%
            dplyr::select(all_of(vector_for_recode)) %>%
            purrr::set_names(names(vector_for_recode)) %>%
            dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
            dplyr::select(database_id, dplyr::everything())
          
          message(paste("Creating", save_in_filename))
          
          standard_dataset %>%
            vroom::vroom_write(save_in_filename)
          
        },
        
        error = function(e) {
          
          message(error_message)
          
          skip_to_next <<- TRUE
          
        }
        
      )
      
      if (skip_to_next) {
        
        next
        
      }
      
    } else {
      
      message(paste(save_in_filename, "already exists!"))
      
    }
    
  }
  
}




############################################################
#                                                          #
#                      01_PRE-FILTER                       #
#                                                          #
############################################################


# coord_trans -------------------------------------------------------------

#' Title: Corrected inverted xy coordinates
#'
#' @param data 
#' @param x 
#' @param y 
#' @param country_code 
#' @param id 
#' @param worldmap 
#' @param worldmap_cntr_code 
#'
#' @return
#' @export
#'
#' @examples
coord_trans <-
  function(data,
           x,
           y,
           country_code,
           id,
           worldmap,
           worldmap_cntr_code
  ) {
    
    data <- data %>% dplyr::select(x, y, country_code, id)
    d1 <- data.frame(x = data[, x], y = -data[, y])
    d2 <- data.frame(x = -data[, x], y = data[, y])
    d3 <- data.frame(x = -data[, x], y = -data[, y])
    d4 <- data.frame(x = data[, y], y = data[, x])
    d5 <- data.frame(x = data[, y], y = -data[, x])
    d6 <- data.frame(x = -data[, y], y = data[, x])
    d7 <- data.frame(x = -data[, y], y = -data[, x])
    
    d.list <- list(d1, d2, d3, d4, d5, d6, d7)
    rm(list = paste0("d", 1:7))
    d.list <- lapply(d.list, function(x) {
      colnames(x) <- c("x", "y")
      return(x)
    })
    
    over_list <- list()
    
    for (d in 1:length(d.list)) {
      caluse <- sp::SpatialPoints(d.list[[d]])
      caluse@proj4string <- worldmap@proj4string
      overresult <- sp::over(caluse, worldmap)
      colnames(d.list[[d]]) <- c(paste0(x, "_modified"), paste0(y, "_modified"))
      over_list[[d]] <- data.frame(d.list[[d]], data, overresult)
      rm(caluse)
      filt <-
        which(over_list[[d]][country_code] == over_list[[d]][worldmap_cntr_code])
      if (length(filt) > 0) {
        over_list[[d]] <- over_list[[d]][filt, ]
      } else {
        over_list[[d]] <- NULL
      }
      rm(list = c("caluse", "overresult", "filt"))
    }
    
    rm(d.list)
    
    over_list <- over_list[!sapply(over_list <- over_list, is.null)]
    over_list <- dplyr::bind_rows(over_list)
    
    return(over_list)
  }



# extract_cntr_names ------------------------------------------------------

#' Title: Extract_cntr_names is a function used to extracting country names in different names from wikipedia in different languages
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
extract_cntr_names <- function(x) {
  if (stringr::str_detect(x, "Note")) {
    x <- stringr::str_split(x, "Note")[[1]][1]
  }
  if (stringr::str_detect(x, "[*]")) {
    x <- stringr::str_split(x, "[*]")[[1]][1]
  }
  if (stringr::str_detect(x, "Alternate, older forms")) {
    x <- stringr::str_split(x, "Alternate, older forms")[[1]][1]
  }
  x <-
    stringr::str_split(x, pattern = "[)]")[[1]] %>%
    stringr::str_split_fixed(., pattern = "[(]", n = 2)
  x <- x[, 1]
  x <-
    x %>%
    stringr::str_split(., pattern = ", ") %>%
    unlist() %>%
    stringr::str_split(., pattern = " ,") %>%
    unlist() %>%
    stringr::str_split(., pattern = ",") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE)
  
  x2 <- x[!str_detect(x, "/|-")]
  
  x3.1 <- x[str_detect(x, "/")] %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim()
  x3.2 <- x[str_detect(x, "-")] %>%
    stringr::str_split(., pattern = " -", n = 2) %>%
    unlist() %>%
    stringr::str_trim()
  
  x <- c(x2, x3.1, x3.2) %>%
    sort() %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " -") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "- ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = ". ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "[\n]") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE) %>%
    sort() %>%
    unique()
  
  x <- x[!(str_length(x) == 1 & grepl(".", x))]
  if (any(x == "Afghanistan")) {
    x <- x[-1]
  }
  x <- x %>%
    data.frame() %>%
    as_tibble()
  
  return(x) # Country name in different language
}



# standard_country --------------------------------------------------------

#' Title: standard_country is a function to correct, standardize, and assign a ISO code to country names 
#'
#' @param data 
#' @param cntry 
#' @param cntry_names_db 
#'
#' @return
#' @export
#'
#' @examples
standard_country <-
  function(data,
           cntry,
           cntry_names_db
  ) {
    # Create a country database based on occ database
    cntr_db <-
      data %>%
      dplyr::distinct_(cntry, .keep_all = FALSE) %>%
      dplyr::arrange_(cntry) %>%
      rename(cntr_original = cntry)
    
    cntr_db$cntr_original2 <-
      stringr::str_replace_all(cntr_db$cntr_original, "[[:punct:]]", " ") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower()
    
    cntr_db <- cntr_db %>% mutate(cntr_suggested = NA)
    
    # Assign country names based on different character matching.
    cntry_names_db <-
      cntry_names_db %>%
      mutate(names_2 = names_in_different_languages %>%
               stringi::stri_trans_general("Latin-ASCII") %>%
               tolower())
    
    cn <- cntry_names_db %>%
      dplyr::distinct(english_name) %>%
      pull(1)
    for (i in 1:length(cn)) {
      cntry_names_db_name <-
        cntry_names_db %>%
        dplyr::filter(english_name == cn[i]) %>%
        pull(names_2)
      
      filt <-
        which(tolower(cntr_db$cntr_original2) %in% tolower(cntry_names_db_name))
      
      if (length(filt) > 0) {
        message("country found: ", cn[i])
        cntr_db$cntr_suggested[filt] <- toupper(cn[i])
      }
    }
    
    cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested)] <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested)],
                                       fuzzyDist = 1,
                                       nthreads = 1)
    
    # Standardization of all names founds in cntr_suggested
    cntr_db$cntr_suggested2 <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_suggested,
                                       fuzzyDist = 1,
                                       nthreads = 1)
    cntr_db <-
      cntr_db %>% 
      mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))
    
    # Second standardization of all names cntr_original2 not
    cntr_db$cntr_suggested2[is.na(cntr_db$cntr_suggested)] <-
      rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested2)],
                                       fuzzyDist = 1,
                                       nthreads = 1)
    cntr_db <-
      cntr_db %>% mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))
    
    
    
    # Country code based on iso2c (it is possible use another code like iso3c, see ?codelist)
    cntr_db$cntr_iso2c <-
      countrycode::countrycode(
        cntr_db$cntr_suggested,
        origin = "country.name.en",
        destination = "iso2c",
        warn = FALSE
      )
    
    cntr_db <-
      cntr_db %>%
      dplyr::select(-cntr_original2, -cntr_suggested) %>%
      dplyr::rename(cntr_suggested = cntr_suggested2)
    
    # data <- left_join(data, cntr_db, by=c('country'="cntr_original"))
    return(cntr_db)
  }



# correct_coordinates -----------------------------------------------------

#' Title: correct_coordinates is a function that will detect those occurrences georreferenced outside their country different coordinate transformation
#'
#' @param data 
#' @param x 
#' @param y 
#' @param sp 
#' @param id 
#' @param cntr_iso2 
#' @param world_poly 
#' @param world_poly_iso 
#'
#' @return
#' @export
#'
#' @examples
correct_coordinates <-
  function(data,
           x,
           y,
           sp,
           id,
           cntr_iso2,
           world_poly,
           world_poly_iso) {
    
    x_mod <- paste0(x, "_modified")
    y_mod <- paste0(y, "_modified")
    
    occ_country <- data %>% dplyr::filter(!is.na(data[cntr_iso2]))
    
    # Filter occurrences database to avoid error in clean_coordiantes errors
    occ_country <-
      occ_country %>%
      dplyr::filter(!is.na(occ_country[x]) |
                      !is.na(occ_country[y]))
    occ_country <-
      occ_country %>%
      dplyr::filter(occ_country[x] >= -180,
                    occ_country[x] <= 180,
                    occ_country[y] >= -90,
                    occ_country[y] <= 90)
    
    
    # Detect those record georeferenced outside a country
    occ_country <- CoordinateCleaner::clean_coordinates(
      x =  occ_country,
      lon = x,
      lat = y,
      species = sp,
      countries = cntr_iso2,
      # iso2 code column of our database
      tests = c("seas", "countries"),
      #Will be tested records located in the see and outside georeferenced countries
      country_ref = world_poly,
      #Here we are using a high resolution countries border database
      country_refcol = world_poly_iso,
      #iso2 code column of country polygon database
      seas_ref = world_poly,
      #Here we are using a high resolution countries border database
      value = "spatialvalid"
    )
    
    summary(occ_country)
    
    # Separate those records outside their countries
    occ_country <- 
      occ_country %>%
      as_tibble() %>%
      dplyr::filter(!.summary,!is.na(occ_country[cntr_iso2]))
    
    message(occ_country %>% nrow, " ocurrences will be tested") #now this database have all those records with potential error that will try to correct
    
    # Split database
    occ_country <-
      occ_country %>% dplyr::group_by_(cntr_iso2) %>% group_split()
    
    
    # coord_trans() function will try different coordinate transformations to correct georeferenced occurrences
    coord_test <- list()
    
    for (i in 1:length(occ_country)) {
      message('Processing occurrence from: ',
              occ_country[[i]][cntr_iso2] %>% unique,
              paste0(" (", nrow(occ_country[[i]]), ")"))
      try(coord_test[[i]] <-
            coord_trans(
              data = occ_country[[i]],
              x = x,
              y = y,
              country_code = cntr_iso2,
              id = id,
              worldmap = world_poly,
              worldmap_cntr_code = world_poly_iso
            ))
    }
    
    filt <- sapply(coord_test, function(x)
      nrow(x) > 0)
    coord_test <-
      coord_test[filt] # elimination from the list those countries without correction
    
    # Elimination of those records near to country border (to avoid flip coordinates or sign that fall too close to country border)
    
    for (i in 1:length(coord_test)) {
      n <- 
        coord_test[[i]] %>%
        dplyr::select_(cntr_iso2) %>% 
        unique %>% 
        pull
      
      my_country <-
        world_poly[which(world_poly@data[, world_poly_iso] == n),] #Here filter polygon based on your country iso2c code
      my_country2 <-
        raster::buffer(my_country, width = 0.5) #0.5 degree ~50km near to equator
      
      coord_sp <- sp::SpatialPoints(coord_test[[i]] %>%
                                      dplyr::select_(x, y))
      
      coord_sp@proj4string <- my_country2@proj4string
      over_occ <- sp::over(coord_sp, my_country2)
      
      # plot(my_country)
      # plot(my_country2, add = T)
      # coord_test[[i]] %>%
      #   dplyr::filter(over_occ == 1) %>%
      #   dplyr::select_(x, y) %>%
      #   points(., pch = 19, col = 'red')
      
      # Eliminate as corrected those records too close to country border
      coord_test[[i]] <-
        coord_test[[i]] %>% dplyr::filter(is.na(over_occ))
    }
    
    # Elimination of those records with more than two possible correction
    coord_test <-
      dplyr::bind_rows(coord_test) %>% 
      as_tibble() # binding dataframes allocated in the list in a single one
    
    coord_test <-
      coord_test %>%
      dplyr::distinct_(., id, .keep_all = T) %>%
      as_tibble %>%
      dplyr::relocate(id, x, y)
    
    # Merge coord_test with other columns of occurrence database
    coord_test <-
      left_join(coord_test,
                data %>% dplyr::select(-c(x, y, cntr_iso2)),
                by = id)
    
    return(coord_test)
  }


# get wiki countries ------------------------------------------------------

# 
#' Title. Wrapped funciton to get cuntry names from Wikipedia
#'
#' @return
#' @export
#'
#' @examples
get_wiki_country <- function() {
  
  wiki_cntr <-
    here::here("data", "wiki_country_names.txt") %>%
    vroom::vroom()
  
  return(wiki_cntr)
  
}


# get worldmap ------------------------------------------------------------

#' Title: FIXEME ####################################
#'
#' @return
#' @export
#'
#' @examples
get_world_map <- function() {
  
  worldmap <- rnaturalearth::ne_countries(scale='large') 
  
  # worldmap@data
  
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
  
  return(worldmap)
  
}




############################################################
#                                                          #
#                       02_TAXONOMY                        #
#                                                          #
############################################################


# rem_family_names --------------------------------------------------------

#' Title: Remove family names from species names
#'
#' @param sp_names 
#'
#' @return
#' @export
#'
#' @examples
rem_family_names <- function(sp_names) {
  sp_names_raw <- sp_names
  df <- data.frame(str_count(sp_names, "\\S+"), sp_names)
  n_string <- ifelse(df[, 1] < 2, FALSE, TRUE)
  n_string <- ifelse(is.na(n_string), FALSE, n_string)
  
  w_rem <- which(n_string == TRUE)
  
  rem_fam <- str_replace_all(
    sp_names[w_rem],
    regex("^[a-zA-Z]+aceae|Leguminosae|Compositae",
          ignore_case = TRUE),
    replacement = " "
  )
  
  rem_fam <- str_squish(rem_fam)
  sp_names[w_rem] <- rem_fam
  
  fag_family <- sp_names_raw == sp_names
  return(data.frame(sp_names, fag_family))
}



# rem_taxo_unc ------------------------------------------------------------

#' Title: Remove terms denoting taxonomic uncertainty (e.g., cf., sp., aff)
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
rem_taxo_unc <- function(spp_names) {
  # confer
  spp_names_clean <-
    str_replace_all(
      spp_names,
      regex("^(cf\\.|cf\\s|cf$)",
        ignore_case = TRUE),
      replacement = " ") # at the beginning

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\scf\\.|\\scf\\s|\\scf$",
      ignore_case = TRUE),
    replacement = " ") # anywhere

  # affinis
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(aff\\.|aff\\s|aff$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\saff\\.|\\saff\\s|\\saff$",
      ignore_case = TRUE),
    replacement = " ")

  # complex
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(complex\\s|complexo|complex$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
      ignore_case = TRUE),
    replacement = " ")

  # genus novum | genus species
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(gen\\.|gen\\s|gen$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sgen\\.|\\sgen\\s|\\sgen$",
      ignore_case = TRUE),
    replacement = " ")

  # species | species (plural)
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$",
      ignore_case = TRUE),
    replacement = " ")

  # species incerta
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
      ignore_case = TRUE),
    replacement = " ")

  # species inquirenda
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(inq\\.|inq\\s|inq$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sinq\\.|\\sinq\\s|\\sinq$",
      ignore_case = TRUE),
    replacement = " ")

  # species indeterminabilis
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
      ignore_case = TRUE),
    replacement = " ")

  # species nova
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(nov\\.|nov\\s|nov$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\snov\\.|\\snov\\s|\\snov$",
      ignore_case = TRUE),
    replacement = " ")

  # species proxima
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
      ignore_case = TRUE),
    replacement = " ")

  # stetit
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(stet\\.|stet\\s|stet$)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sstet\\.|\\sstet\\s|\\sstet$",
      ignore_case = TRUE),
    replacement = " ")

  # hybrids
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(x\\s)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sx\\s|\\sx$",
      ignore_case = TRUE),
    replacement = " ")

  # forma
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sf\\.|\\sf\\s|\\f$|\\sfo\\.|\\fo\\s",
      ignore_case = TRUE),
    replacement = " ")

  # Non-available (NA)
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sna\\.|\\sna\\s|\\sna$",
      ignore_case = TRUE),
    replacement = " ")

  # sem
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(sem\\s|sem\\.)",
      ignore_case = TRUE),
    replacement = " ")

  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\ssem\\.|\\ssem\\s|\\ssem$",
      ignore_case = TRUE),
    replacement = " ")

  # Remove extra spaces
  spp_names_clean <- str_squish(spp_names_clean)
  return(spp_names_clean)
}



# flag_taxo_unc -----------------------------------------------------------

#' Title: Flag terms denoting taxonomic uncertainty (T = no terms found; F = term found)
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
flag_taxo_unc <- function(spp_names) {
  # confer
  cf0 <- str_detect(
    spp_names,
    regex("^(cf\\.|cf\\s|cf$)",
      ignore_case = TRUE)) # at the beginning

  cf <- str_detect(
    spp_names,
    regex("\\scf\\.|\\scf\\s|\\scf$",
      ignore_case = TRUE)) # anywhere

  # affinis
  aff0 <- str_detect(
    spp_names,
    regex("^(aff\\.|aff\\s|aff$)",
      ignore_case = TRUE))

  aff <- str_detect(
    spp_names,
    regex("\\saff\\.|\\saff\\s|\\saff$",
      ignore_case = TRUE))

  # complex
  complex0 <- str_detect(
    spp_names,
    regex("^(complex\\s|complexo|complex$)",
      ignore_case = TRUE))

  complex <- str_detect(
    spp_names,
    regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
      ignore_case = TRUE))

  # genus novum | genus species
  gen0 <- str_detect(
    spp_names,
    regex("^(gen\\.|gen\\s|gen$)",
      ignore_case = TRUE))

  gen <- str_detect(
    spp_names,
    regex("\\sgen\\.|\\sgen\\s|\\sgen$",
      ignore_case = TRUE))


  # species | species (plural)
  sp0 <- str_detect(
    spp_names,
    regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$)",
      ignore_case = TRUE))

  sp <- str_detect(
    spp_names,
    regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$",
      ignore_case = TRUE))

  # species incerta
  sp_inc0 <- str_detect(
    spp_names,
    regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
      ignore_case = TRUE))

  sp_inc <- str_detect(
    spp_names,
    regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
      ignore_case = TRUE))

  # species inquirenda
  sp_inq0 <- str_detect(
    spp_names,
    regex("^(inq\\.|inq\\s|inq$)",
      ignore_case = TRUE))

  sp_inq <- str_detect(
    spp_names,
    regex("\\sinq\\.|\\sinq\\s|\\sinq$",
      ignore_case = TRUE))

  # species indeterminabilis
  sp_indet0 <- str_detect(
    spp_names,
    regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE))

  sp_indet <- str_detect(
    spp_names,
    regex(
      "\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
      ignore_case = TRUE))

  # species nova
  sp_nova0 <- str_detect(
    spp_names,
    regex("^(nov\\.|nov\\s|nov$)",
      ignore_case = TRUE))

  sp_nova <- str_detect(
    spp_names,
    regex("\\snov\\.|\\snov\\s|\\snov$",
      ignore_case = TRUE))

  # species proxima
  sp_proxima0 <- str_detect(
    spp_names,
    regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
      ignore_case = TRUE))

  sp_proxima <- str_detect(
    spp_names,
    regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
      ignore_case = TRUE))

  # stetit
  stet0 <- str_detect(
    spp_names,
    regex("^(stet\\.|stet\\s|stet$)",
      ignore_case = TRUE))

  stet <- str_detect(
    spp_names,
    regex("\\sstet\\.|\\sstet\\s|\\sstet$",
      ignore_case = TRUE))

  terms <-
    c(
      "cf0", "cf", "aff0", "aff", "complex0", "complex", "gen0", "gen",
      "sp0", "sp", "sp_inc0", "sp_inc", "sp_inq0",
      "sp_inq", "sp_indet0", "sp_indet", "sp_nova0", "sp_nova",
      "sp_proxima0", "sp_proxima", "stet0", "stet"
    )

  terms_names <-
    c(
      "confer", "confer", "affinis", "affinis", "complex", "complex",
      "genus novum or genus species", "genus novum or genus species",
      "species", "species", "species incerta",
      "species incerta", "species inquirenda", "species inquirenda",
      "species indeterminabilis", "species indeterminabilis",
      "species nova", "species nova", "species proxima", "species proxima",
      "stetit", "stetit"
    )


  term_uncertainty <- rep(NA, length(spp_names))
  taxo_uncertainty <- rep(FALSE, length(spp_names))

  for (i in 1:length(terms)) {
    t <- get(terms[i])
    posi <- which(t == TRUE)
    term_uncertainty[posi] <- terms_names[i]
    taxo_uncertainty[posi] <- TRUE
  }

  taxo_uncertainty <- ifelse(taxo_uncertainty == TRUE, FALSE, TRUE)
  tab_res <- as.data.frame(cbind(term_uncertainty, taxo_uncertainty))
  w <- which(is.na(spp_names))
  tab_res[w, "taxo_uncertainty"] <- NA

  return(tab_res)
}



# rem_other_issues --------------------------------------------------------

<<<<<<< HEAD
#' Title: Remove duplicated genus, substitute empty cells by NA, capitalize generic name
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
rem_other_issues <- function(spp_names) {
=======
# Remove duplicated genus, substitute empty cells by NA, capitalize generic name
rem_other_issues <- function(spp_names) {
  rem_dup_names <- function(x) {
    res <- x
    word_count <- stringr::str_count(x, "\\w+")
    
    w1 <- which(word_count == 1)
    res[w1] <- stringr::str_to_lower(res[w1])
    res[w1] <- Hmisc::capitalize(res[w1])
    
    w3 <- which(word_count >= 3)
    
    for (i in w3)
    {
      sp <- x[i]
      sp <- gsub(pattern = "-", " ", sp)
      t <- trimws(sp)
      u <- unlist(
        strsplit(t, split = " ", fixed = F, perl = T)
      )
      dup <- paste(tolower(unique(c(u[1], u[2]))), sep = " ", collapse = " ")
      remain <- paste(u[3:length(u)], sep = " ", collapse = " ")
      p <- paste(dup, remain)
      res[i] <- p
    }
    return(res)
  }
  remDup <- rem_dup_names(spp_names)
  v3 <- gsub("^$", NA, remDup) # substitue empty records by NA
  v4 <- Hmisc::capitalize(v3)
  return(v4)
}


# Query names in WorldFlora Online database
query_wfo <- function(species_name, match_dist) {
  
  # Download WFO database
  wfo_data <- here::here("data", "WFO_Backbone", "classification.txt")
  wfo_dir <- here::here("data", "WFO_Backbone")
  
  if (!fs::file_exists(wfo_data)) {
    fs::dir_create(wfo_dir)
    
    WorldFlora::WFO.download(
      WFO.url = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip",
      save.dir = wfo_dir,
      WFO.remember = T
    )
    
    utils::unzip("data/WFO_Backbone/WFO_Backbone.zip", exdir = wfo_dir)
  }
  
  query_names <-
    WorldFlora::WFO.match(species_name,
                          WFO.file = "data/WFO_Backbone/classification.txt",
                          Fuzzy.min = T,
                          squish = F,
                          spec.name.nonumber = F,
                          spec.name.nobrackets = F,
                          spec.name.sub = F,
                          verbose = F,
                          counter = T
    )
  
  query_names_filter <-
    query_names %>%
    dplyr::select(Fuzzy.dist, taxonRank, scientificName, spec.name) %>%
    mutate(query_match_dist = if_else(Fuzzy.dist <= match_dist |
                                        is.na(Fuzzy.dist), T, F)) %>%
    dplyr::mutate(scientificName = ifelse(query_match_dist == T,
                                          scientificName,
                                          NA
    ))
  return(query_names_filter)
}



############################################################
#                                                          #
#                          SPACE                           #
#                                                          #
############################################################


# coord_trans -------------------------------------------------------------

# Corrected inverted coordinates
coord_trans <-
  function(data,
           x,
           y,
           country_code,
           id,
           worldmap,
           worldmap_cntr_code
  ) {
  
  data <- data %>% dplyr::select(x, y, country_code, id)
  d1 <- data.frame(x = data[, x], y = -data[, y])
  d2 <- data.frame(x = -data[, x], y = data[, y])
  d3 <- data.frame(x = -data[, x], y = -data[, y])
  d4 <- data.frame(x = data[, y], y = data[, x])
  d5 <- data.frame(x = data[, y], y = -data[, x])
  d6 <- data.frame(x = -data[, y], y = data[, x])
  d7 <- data.frame(x = -data[, y], y = -data[, x])

  d.list <- list(d1, d2, d3, d4, d5, d6, d7)
  rm(list = paste0("d", 1:7))
  d.list <- lapply(d.list, function(x) {
    colnames(x) <- c("x", "y")
    return(x)
  })

  over_list <- list()

  for (d in 1:length(d.list)) {
    caluse <- sp::SpatialPoints(d.list[[d]])
    caluse@proj4string <- worldmap@proj4string
    overresult <- sp::over(caluse, worldmap)
    colnames(d.list[[d]]) <- c(paste0(x, "_modified"), paste0(y, "_modified"))
    over_list[[d]] <- data.frame(d.list[[d]], data, overresult)
    rm(caluse)
    filt <-
      which(over_list[[d]][country_code] == over_list[[d]][worldmap_cntr_code])
    if (length(filt) > 0) {
      over_list[[d]] <- over_list[[d]][filt, ]
    } else {
      over_list[[d]] <- NULL
    }
    rm(list = c("caluse", "overresult", "filt"))
  }

  rm(d.list)

  over_list <- over_list[!sapply(over_list <- over_list, is.null)]
  over_list <- dplyr::bind_rows(over_list)
  
  return(over_list)
}



# extract_cntr_names ------------------------------------------------------

# extract_cntr_names is a function used to extracting country names in different names from wikipedia in different languages

extract_cntr_names <- function(x) {
  if (stringr::str_detect(x, "Note")) {
    x <- stringr::str_split(x, "Note")[[1]][1]
  }
  if (stringr::str_detect(x, "[*]")) {
    x <- stringr::str_split(x, "[*]")[[1]][1]
  }
  if (stringr::str_detect(x, "Alternate, older forms")) {
    x <- stringr::str_split(x, "Alternate, older forms")[[1]][1]
  }
  x <-
    stringr::str_split(x, pattern = "[)]")[[1]] %>%
    stringr::str_split_fixed(., pattern = "[(]", n = 2)
  x <- x[, 1]
  x <-
    x %>%
    stringr::str_split(., pattern = ", ") %>%
    unlist() %>%
    stringr::str_split(., pattern = " ,") %>%
    unlist() %>%
    stringr::str_split(., pattern = ",") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE)

  x2 <- x[!str_detect(x, "/|-")]
  
  x3.1 <- x[str_detect(x, "/")] %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim()
  x3.2 <- x[str_detect(x, "-")] %>%
    stringr::str_split(., pattern = " -", n = 2) %>%
    unlist() %>%
    stringr::str_trim()

  x <- c(x2, x3.1, x3.2) %>%
    sort() %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " -") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "- ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = ". ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "[\n]") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE) %>%
    sort() %>%
    unique()

  x <- x[!(str_length(x) == 1 & grepl(".", x))]
  if (any(x == "Afghanistan")) {
    x <- x[-1]
  }
  x <- x %>%
    data.frame() %>%
    as_tibble()

  return(x) # Country name in different language
}



# standard_country --------------------------------------------------------

# standard_country is a function to correct, standardize, and assign a ISO code to country names 

standard_country <-
  function(data,
           cntry,
           cntry_names_db
  ) {
  # Create a country database based on occ database
  cntr_db <-
    data %>%
    dplyr::distinct_(cntry, .keep_all = FALSE) %>%
    dplyr::arrange_(cntry) %>%
    rename(cntr_original = cntry)

  cntr_db$cntr_original2 <-
    stringr::str_replace_all(cntr_db$cntr_original, "[[:punct:]]", " ") %>%
    str_trim() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower()

  cntr_db <- cntr_db %>% mutate(cntr_suggested = NA)

  # Assign country names based on different character matching.
  cntry_names_db <-
    cntry_names_db %>%
    mutate(names_2 = names_in_different_languages %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      tolower())

  cn <- cntry_names_db %>%
    dplyr::distinct(english_name) %>%
    pull(1)
  for (i in 1:length(cn)) {
    cntry_names_db_name <-
      cntry_names_db %>%
      dplyr::filter(english_name == cn[i]) %>%
      pull(names_2)
    
    filt <-
      which(tolower(cntr_db$cntr_original2) %in% tolower(cntry_names_db_name))
    
    if (length(filt) > 0) {
      message("country found: ", cn[i])
      cntr_db$cntr_suggested[filt] <- toupper(cn[i])
    }
  }

  cntr_db$cntr_suggested[is.na(cntr_db$cntr_suggested)] <-
    rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested)],
                                     fuzzyDist = 1,
                                     nthreads = 1)

  # Standardization of all names founds in cntr_suggested
  cntr_db$cntr_suggested2 <-
    rangeBuilder::standardizeCountry(cntr_db$cntr_suggested,
                                     fuzzyDist = 1,
                                     nthreads = 1)
  cntr_db <-
    cntr_db %>% 
    mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))

  # Second standardization of all names cntr_original2 not
  cntr_db$cntr_suggested2[is.na(cntr_db$cntr_suggested)] <-
    rangeBuilder::standardizeCountry(cntr_db$cntr_original2[is.na(cntr_db$cntr_suggested2)],
                                     fuzzyDist = 1,
                                     nthreads = 1)
  cntr_db <-
    cntr_db %>% mutate(cntr_suggested2 = ifelse(cntr_suggested2 == "", NA, cntr_suggested2))



  # Country code based on iso2c (it is possible use another code like iso3c, see ?codelist)
  cntr_db$cntr_iso2c <-
    countrycode::countrycode(
      cntr_db$cntr_suggested,
      origin = "country.name.en",
      destination = "iso2c",
      warn = FALSE
    )

  cntr_db <-
    cntr_db %>%
    dplyr::select(-cntr_original2, -cntr_suggested) %>%
    dplyr::rename(cntr_suggested = cntr_suggested2)

  # data <- left_join(data, cntr_db, by=c('country'="cntr_original"))
  return(cntr_db)
}



# correct_coordinates -----------------------------------------------------

# correct_coordinates is a function that will detect those occurrences georreferenced outside their country by testing with different coordinate transformation

correct_coordinates <-
  function(data,
           x,
           y,
           sp,
           id,
           cntr_iso2,
           world_poly,
           world_poly_iso) {
    
  x_mod <- paste0(x, "_modified")
  y_mod <- paste0(y, "_modified")
  
  occ_country <- data %>% dplyr::filter(!is.na(data[cntr_iso2]))
  
  # Filter occurrences database to avoid error in clean_coordiantes errors
  occ_country <-
    occ_country %>%
    dplyr::filter(!is.na(occ_country[x]) |
                    !is.na(occ_country[y]))
  occ_country <-
    occ_country %>%
    dplyr::filter(occ_country[x] >= -180,
                  occ_country[x] <= 180,
                  occ_country[y] >= -90,
                  occ_country[y] <= 90)
  
  
  # Detect those record georeferenced outside a country
  occ_country <- CoordinateCleaner::clean_coordinates(
    x =  occ_country,
    lon = x,
    lat = y,
    species = sp,
    countries = cntr_iso2,
    # iso2 code column of our database
    tests = c("seas", "countries"),
    #Will be tested records located in the see and outside georeferenced countries
    country_ref = world_poly,
    #Here we are using a high resolution countries border database
    country_refcol = world_poly_iso,
    #iso2 code column of country polygon database
    seas_ref = world_poly,
    #Here we are using a high resolution countries border database
    value = "spatialvalid"
  )
  
  summary(occ_country)
  
  # Separate those records outside their countries
  occ_country <- 
    occ_country %>%
    as_tibble() %>%
    dplyr::filter(!.summary,!is.na(occ_country[cntr_iso2]))
  
  message(occ_country %>% nrow, " ocurrences will be tested") #now this database have all those records with potential error that will try to correct
>>>>>>> e387839c8b4672557d8d44e9bc7814ea3f51cf0c
  
  res <- spp_names
  word_count <- stringr::str_count(res, "\\w+")
  
  w1 <- which(word_count == 1)
  res[w1] <- stringr::str_to_lower(res[w1])
  res[w1] <- Hmisc::capitalize(res[w1])
  
  w3 <- which(word_count >= 3)
  
  for (i in w3)
  {
    sp <- spp_names[i]
    sp <- tolower(spp_names[i])
    sp <- gsub(pattern = "-", " ", sp)
    t <- trimws(sp)
    u <- unlist(
      strsplit(t, split = " ", fixed = F, perl = T)
    )
    dup <- paste(tolower(unique(c(u[1], u[2]))), sep = " ", collapse = " ")
    remain <- paste(u[3:length(u)], sep = " ", collapse = " ")
    p <- paste(dup, remain)
    res[i] <- p
  }
  v3 <- gsub("^$", NA, res) # substitute empty records by NA
  v4 <- Hmisc::capitalize(v3)
  return(v4)
}



# query_wfo ---------------------------------------------------------------

#' Title: Query names in WorldFlora Online database
#'
#' @param species_name 
#' @param match_dist 
#'
#' @return
#' @export
#'
#' @examples
query_wfo <- function(species_name, match_dist) {
  
  # Download WFO database
  wfo_data <- here::here("data", "WFO_Backbone", "classification.txt")
  wfo_dir <- here::here("data", "WFO_Backbone")
  
  if (!fs::file_exists(wfo_data)) {
    fs::dir_create(wfo_dir)
    
    WorldFlora::WFO.download(
      WFO.url = "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip",
      save.dir = wfo_dir,
      WFO.remember = T
    )
    
    utils::unzip("data/WFO_Backbone/WFO_Backbone.zip", exdir = wfo_dir)
  }
  
  query_names <-
    WorldFlora::WFO.match(species_name,
                          WFO.file = "data/WFO_Backbone/classification.txt",
                          Fuzzy.min = T,
                          squish = F,
                          spec.name.nonumber = F,
                          spec.name.nobrackets = F,
                          spec.name.sub = F,
                          verbose = F,
                          counter = T
    )
  
  query_names_filter <-
    query_names %>%
    dplyr::select(Fuzzy.dist, taxonRank, scientificName, spec.name) %>%
    mutate(query_match_dist = if_else(Fuzzy.dist <= match_dist |
                                        is.na(Fuzzy.dist), T, F)) %>%
    dplyr::mutate(scientificName = ifelse(query_match_dist == T,
                                          scientificName,
                                          NA
    ))
  return(query_names_filter)
}



# get.taxa.taxadb ---------------------------------------------------------

#' Title: Get names using taxadb R package. Fuzzy match is allowed by using a modification version of taxadb get_filter names function
#'
#' @param taxa 
#' @param replace.synonyms 
#' @param suggest.names 
#' @param suggestion.distance 
#' @param parse 
#' @param db 
#'
#' @return
#' @export
#'
#' @examples
get.taxa.taxadb <-
  function (taxa,
            replace.synonyms = TRUE,
            suggest.names = TRUE,
            suggestion.distance = 0.9,
            parse = FALSE,
            db = NULL) {
    taxa <- trim(taxa)
    taxa <- taxa[nzchar(taxa)]
    if (length(taxa) == 0L) 
      stop("No valid names provided.")
    original.search <- taxa
    col_names <- suppressWarnings(colnames(taxadb::filter_name(NA, provider = db)))
    ncol.taxa <- length(col_names)
    res <- data.frame(matrix(vector(), length(taxa), ncol.taxa + 
                               3, dimnames = list(c(), c(col_names, "notes", "original.search", "distance"))), 
                      stringsAsFactors = FALSE)
    minus.notes <- seq_len(ncol.taxa)
    index <- 0
    for (taxon in taxa) {
      notes <- NULL
      index <- index + 1
      if (parse) {
        url <- "http://api.gbif.org/v1/parser/name"
        request <- try(POST(url, body = list(taxon), encode = "json"))
        if (inherits(request, "try-error")) {
          warning("Couldn't connect with the GBIF data servers. Check your internet connection or try again later.")
        }
        else {
          warn_for_status(request)
          taxon <- content(request)[[1]]$canonicalName
        }
      }
      taxon <- fixCase(taxon)
      uncertain <- regmatches(taxon, regexpr("[a|c]f+\\.", 
                                             taxon))
      if (length(uncertain) != 0L) {
        taxon <- gsub("\\s[a|c]f+\\.", "", taxon)
      }
      ident <- regmatches(taxon, regexpr("\\s+sp\\.+\\w*", 
                                         taxon))
      if (length(ident) != 0L) {
        split.name <- unlist(strsplit(taxon, " "))
        taxon <- split.name[1]
        infra <- split.name[2]
      }
      found <- !is.na(suppressWarnings(taxadb::get_ids(taxon, db = db)))
      
      if (!found) {
        if (suggest.names) {
          suggested <- suggest.names.taxadb(taxon, max.distance = suggestion.distance, provide = db)
          taxon <- suggested[1]
          res[index, "distance"] <- suggested[2]
        }
        else {
          res[index, "notes"] <- "not found"
          next
        }
        if (is.na(taxon)) {
          res[index, "notes"] <- "not found"
          next
        }
        else {
          notes <- "was misspelled"
        }
      }
      found_name <-suppressWarnings(taxadb::filter_name(taxon, provider = db)) 
      n_found <- sum(found_name$taxonomicStatus =="accepted")
      
      if (n_found > 0) {
        if (n_found == 1L) {
          res[index, minus.notes] <- found_name
        }
        else {
          notes <- c(notes, "check +1 accepted")
        }
        res[index, "notes"] <- paste(notes, collapse = "|")
        res[index, "original.search"] <- original.search
        next
      }
      
      nrow.synonym <- sum(found_name$taxonomicStatus =="synonym")
      if (nrow.synonym > 0L) {
        if (replace.synonyms) {
          accepted <- suppressWarnings(taxadb::get_names(found_name$acceptedNameUsageID, db)) 
          nrow.accepted <- sum(!is.na(accepted))
          if (nrow.accepted == 0L) {
            if (nrow.synonym == 1L) {
              notes <- c(notes, "check no accepted name")
              res[index, minus.notes] <- found_name
            }
            if (nrow.synonym > 1L) {
              notes <- c(notes, "check no accepted +1 synonyms")
            }
          }
          if (nrow.accepted == 1L) {
            notes <- c(notes, "replaced synonym")
            replace <- suppressWarnings(taxadb::filter_name(accepted, provider = db)) 
            res[index, minus.notes] <- replace 
          }
          if (nrow.accepted > 1L) {
            notes <- c(notes, "check +1 accepted")
            if (nrow.synonym == 1L) {
              res[index, minus.notes] <- found_name
            }
          }
        }
        else {
          if (nrow.synonym == 1L) {
            res[index, minus.notes] <- found_name
          }
          if (nrow.synonym > 1L) {
            notes <- c(notes, "check +1 entries")
          }
        }
        res[index, "notes"] <- paste(notes, collapse = "|")
        res[index, "original.search"] <- original.search
        next
      }
    }
    res
  }


# suggest.names.taxadb ----------------------------------------------------

#' Title: Function used to standardized taxonomic names using taxadb R packpage. This functions is a modification of get.taxa function (flora package) inserted in get_names function (taxadb package) for allowing fuzzy matching.
#'
#' @param taxon 
#' @param max.distance 
#' @param return.na 
#' @param ignore.words 
#' @param provider 
#'
#' @return
#' @export
#'
#' @examples
suggest.names.taxadb <-
  function (taxon,
            max.distance = 0.75,
            return.na = TRUE,
            ignore.words = NULL,
            provider
            
  ) {
    taxon <- fixCase(taxon)
    taxon.orig <- taxon
    uncertain <- regmatches(taxon, regexpr("[a|c]f+\\.", 
                                           taxon))
    taxon <- gsub("^\\s+|\\s+$", "", taxon)
    if (length(uncertain) != 0L) 
      taxon <- gsub("[a|c]f+\\.", "", taxon)
    ident <- regmatches(taxon, regexpr("\\s+sp\\.+\\w*", 
                                       taxon))
    if (length(ident) != 0L) 
      taxon <- unlist(strsplit(taxon, " "))[1]
    if (!nzchar(taxon)) 
      return(NA)
    
    first.letter <- strsplit(taxon, "")[[1]][1]
    species.first.letter <- suppressWarnings(taxadb::name_starts_with(first.letter, provider = provider))[, c( "taxonID", "scientificName", 
                                                                                                               "taxonRank", "taxonomicStatus",         
                                                                                                               "acceptedNameUsageID" )]
    
    
    l1 <- length(taxon)
    l2 <- length(species.first.letter$scientificName)
    out <- stringdist::stringdist(taxon, species.first.letter$scientificName)
    distance <- 1 - (out/pmax(nchar(taxon), nchar(species.first.letter$scientificName)))
    max.dist <- max(distance, na.rm = TRUE)
    if (max.dist >= max.distance) {
      if (length(ident) == 0L) {
        dis <- max(distance, na.rm = TRUE)
        res <- species.first.letter$scientificName[distance == dis][1]
        
        if (length(uncertain) == 0L) {
          
          return(c(res, dis))
        }
        else {
          res <- unlist(strsplit(res, " "))
          return(c(paste(res[1], uncertain, res[2:length(res)]), dis))
        }
      }
      else {
        paste(species.first.letter$scientificName[distance == max(distance, 
                                                                  na.rm = TRUE)][1], ident, sep = "")
      }
    }
    else {
      if (return.na) {
        NA
      }
      else {
        taxon.orig
      }
    }
  }




############################################################
#                                                          #
#                          SPACE                           #
#                                                          #
############################################################




############################################################
#                                                          #
#                     OTHER FUNCTIONS                      #
#                                                          #
############################################################

# quickmap ----------------------------------------------------------------

#' Title: Create a map of points using ggplot2
#'
#' @param data 
#' @param lon 
#' @param lat 
#'
#' @return
#' @export
#'
#' @examples
quickmap <- function(data, lat, lon) {

  n_nrow_data <- format(x = nrow(data), big.mark = ",")

  world_borders <-
    borders(
      database = "world",
      fill = "white",
      colour = "grey90",
    )

  our_map <-
    data %>%
    ggplot() +
    world_borders +
    theme_bw() +
    labs(
      x = "Longitude (decimals)",
      y = "Latitude (decimals)",
      title = paste("Based on ", n_nrow_data, "points")
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_blank()
    ) +
    geom_point(
      aes(
        x = {{ lat }}, 
        y = {{ lon }}
      ), 
      alpha = 0.5,
      size = 0.1
    )

  print(our_map)

}

# filter and save data not in filtered data
# ex: raw_data %>% not_in(filtered_data)



# export_rejected_data ----------------------------------------------------

#' Title
#'
#' @param raw_data 
#' @param filtered_data 
#' @param save_in_filename 
#' @param comment 
#'
#' @return
#' @export
#'
#' @examples
export_rejected_data <- function(raw_data, filtered_data, save_in_filename, comment = NULL) {

  if (!file.exists(save_in_filename)) {

    n_raw_data <- nrow(raw_data)

    n_filtered_data <- nrow(filtered_data)

    rejected_data <-
      anti_join(raw_data, filtered_data, by = "database_id")

    n_rejected_data <- nrow(rejected_data)

    log_file <- here::here("output", "log.csv")

    if (!is.null(comment)) {

      comment <- str_replace_all(comment, ",", ".")

    } else {

      comment <- NA

    }

    prepare_log <-
      data.frame(
        timestamp = Sys.time(),
        raw_data = paste(deparse(substitute(raw_data))),
        nrow_raw_data = n_raw_data,
        filtered_data = paste(deparse(substitute(filtered_data))),
        nrow_filtered_data = n_filtered_data,
        rejected_data = paste(save_in_filename),
        nrow_rejected_daat = n_rejected_data,
        comment = comment
      )

    if (!file.exists(log_file)) {

      log_template <-
        data.frame(
          "timestamp",
          "raw_data",
          "nrow_raw_data",
          "filtered_data",
          "nrow_filtered_data",
          "rejected_data",
          "nrow_rejected_data",
          "comment"
        )

      write_csv(log_template, log_file, append = TRUE)

    }

    message(paste("Saving rejected data in ", save_in_filename))

    write_csv(rejected_data, save_in_filename)

    message(paste("Appending log in ", log_file))

    write_csv(prepare_log, log_file, append = TRUE)

    message(paste("Check latest ", log_file))

    suppressPackageStartupMessages({

      read_csv(log_file)

    })

  } else {

    message(paste(save_in_filename, "already exists!"))

  }

}

