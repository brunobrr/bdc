
#' Remove family names from species names
#'
#' @param data 
#' @param sci_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_family_names <- function(data, sci_names) {
  
  # Get animalia family names from gbif via taxadb package
  animalia_families <-
    taxadb::taxa_tbl("gbif") %>%
    dplyr::filter(kingdom == "Animalia") %>%
    dplyr::select(family) %>%
    dplyr::distinct() %>%
    dplyr::pull(family)
  
  # Raw scientific names
  sci_names_raw <- data[[sci_names]] %>% stringr::str_squish()
  
  # Vector to save scientific names without family names
  clean_family_names <- sci_names_raw
  
  # Count number of words to avoid removing names composed of a single family name
  word_count <- str_count(sci_names_raw, "\\S+")
  df <- data.frame(word_count, sci_names_raw)
  n_string <- ifelse(df[, 1] < 2, TRUE, FALSE)
  n_string <- ifelse(is.na(n_string), TRUE, n_string)
  
  posi <- which(n_string == FALSE)
  
  # Plantae: remove suffix "aceae"
  rem_fam <- stringr::str_remove_all(
    sci_names_raw[posi],
    regex("[a-zA-Z]+aceae|Leguminosae|Compositae",
          ignore_case = TRUE
    )
  ) %>%
    stringr::str_squish()
  
  clean_family_names[posi] <- rem_fam
  .family_names <- sci_names_raw == clean_family_names
  
  
  # Animalia: detect suffix "dae"
  detect_fam_animalia <- stringr::str_detect(
    sci_names_raw[posi],
    regex("[a-zA-Z]+dae", ignore_case = TRUE),
    negate = T
  )
  
  df <-
    data.frame(posi, detect_fam_animalia, sci_names_raw[posi]) %>%
    filter(detect_fam_animalia == FALSE)
  
  # Check against a list of known family names to avoid erroneously flag generic names (e.g. "Solanum lacerdae")
  check <- df$posi
  is_valid <- NULL
  for (i in check) {
    test <- stringr::str_which(
      sci_names_raw[i],
      regex(animalia_families, ignore_case = T)
    )
    test <- ifelse(length(test) == 0, TRUE, FALSE)
    is_valid <- c(is_valid, test)
  }
  
  df <- cbind(df, is_valid)
  
  # Remove suffix "dae"
  if (any(df$is_valid == F)) {
    posi_temp <- which(is_valid == FALSE)
    posi_valid <- check[posi_temp]
    
    rem_fam_animalia <- stringr::str_remove_all(
      sci_names_raw[posi_valid],
      regex("[a-zA-Z]+dae",
            ignore_case = TRUE
      )
    ) %>%
      stringr::str_squish()
    
    clean_family_names[posi_valid] <- rem_fam_animalia
  }
  
  .family_names <- sci_names_raw == clean_family_names
  
  df_final <- data.frame(.family_names, clean_family_names)
  df_final <- dplyr::bind_cols(data, df_final)
  
  message(
    paste(
      "bdc_rem_family_names:\nRemoved and flagged",
      sum(!.family_names),
      "records.\nTwo collumns were added to the database.\n"
    )
  )
  
  return(df_final)
}
