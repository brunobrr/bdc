
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
#                        PRE-FILTER                        #
#                                                          #
############################################################


# bdc_get_wiki_country ----------------------------------------------------
#' 
#' # 
#' #' Title. Wrapped function to get country names from Wikipedia
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bdc_get_wiki_country <- function() {
#' 
#'   wiki_cntr <-
#'     here::here("data", "wiki_country_names.txt") %>%
#'     vroom::vroom()
#' 
#'   return(wiki_cntr)
#' 
#' }


# bdc_get_world_map -------------------------------------------------------

#' Title
#'
#' @return
#' @export
#'
#' @examples
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



## bdc_export_rejected_data ------------------------------------------------

##' Title
##'
##' @param raw_data 
##' @param filtered_data 
##' @param save_in_filename 
##' @param comment 
##'
##' @return
##' @export
##'
##' @examples
## TODO: incluir argumento para salvar arquivo ou não
#bdc_export_data_to_check <- function(raw_data, filtered_data, save_in_filename, save_data_file = FALSE, report_name, comment = NULL) {

#  if (!file.exists(save_in_filename)) {

#    n_raw_data <- nrow(raw_data)

#    n_filtered_data <- nrow(filtered_data)

#    data_to_check <-
#      anti_join(raw_data, filtered_data, by = "database_id")

#    n_rejected_data <- nrow(data_to_check)

#    log_file <- report_name

#    if (!is.null(comment)) {

#      comment <- str_replace_all(comment, ",", ".")

#    } else {

#      comment <- NA

#    }

#    if (save_data_file == FALSE) {

#      save_in_filename <- "data not saved"

#    }

#    prepare_log <-
#      data.frame(
#        timestamp = Sys.time(),
#        raw_data = paste(deparse(substitute(raw_data))),
#        nrow_raw_data = n_raw_data,
#        filtered_data = paste(deparse(substitute(filtered_data))),
#        nrow_filtered_data = n_filtered_data,
#        data_to_check = paste(save_in_filename),
#        nrow_rejected_daat = n_rejected_data,
#        comment = comment
#      )

#    if (!file.exists(log_file)) {

#      log_template <-
#        data.frame(
#          "timestamp",
#          "raw_data",
#          "nrow_raw_data",
#          "filtered_data",
#          "nrow_filtered_data",
#          "data_to_check",
#          "nrow_rejected_data",
#          "comment"
#        )

#      write_csv(log_template, log_file, append = TRUE)

#    }


#    if (save_data_file) {

#      message(paste("Saving data to check in ", save_in_filename))

#      write_csv(data_to_check, save_in_filename)

#    }

#    message(paste("Appending log in ", log_file))

#    write_csv(prepare_log, log_file, append = TRUE)

#    message(paste("Check latest ", log_file))

#    suppressPackageStartupMessages({

#      read_csv(log_file)

#    })

#  } else {

#    message(paste(save_in_filename, "already exists!"))

#  }

#}

bdc_check_flags <- function(data) {

  data %>%
  select(contains(".")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  distinct() %>%
  arrange(name)

}
