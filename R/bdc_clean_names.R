#' Clean scientific names
#'
#' @param sci_names character. Vector of scientific names or a data frame containing the column of names to be parsed.
#' @details The function identifies, flags and removes suffix, infraspecific, uncertain and provisional names. 
#' @return It returns a data frame with clean and unique scientific names.
#' @export
#'
#' @examples
bdc_clean_names <- function(sci_names){
  
  # names raw
  names_raw <-
    sci_names %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(scientificName = value)
  
  # Only unique taxa names will be queried (empty or NA names are excluded)
  names <-
    names_raw %>% 
    dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
    dplyr::select(scientificName) %>% # select this column
    dplyr::mutate_all(na_if,"") %>% # change empty names to NA
    dplyr::filter(!is.na(scientificName)) # remove NAs
  


# Family names ------------------------------------------------------------
  bdc_rem_family_names <- function(data, sci_names) {
    Sys.setenv("CONTENTID_REGISTRIES"  = "https://hash-archive.thelio.carlboettiger.info")
    
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
  
# Uncertainty terms -------------------------------------------------------
  # Flag, identify and remove terms denoting uncertainty or provisional status of a taxonomic identification
  bdc_rem_taxo_unc <- function(data, sci_names) {
    
    spp_names <- data[[sci_names]] %>% stringr::str_squish()
    
    ### Flag and identify uncertainty terms
    
    # confer
    cf0 <- stringr::str_detect(
      spp_names,
      regex("^(cf\\.|cf\\s|cf$)",
            ignore_case = TRUE)) # at the beginning
    
    cf <- stringr::str_detect(
      spp_names,
      regex("\\scf\\.|\\scf\\s|\\scf$",
            ignore_case = TRUE)) # anywhere
    
    # affinis
    aff0 <- stringr::str_detect(
      spp_names,
      regex("^(aff\\.|aff\\s|aff$)",
            ignore_case = TRUE))
    
    aff <- stringr::str_detect(
      spp_names,
      regex("\\saff\\.|\\saff\\s|\\saff$",
            ignore_case = TRUE))
    
    # complex
    complex0 <- stringr::str_detect(
      spp_names,
      regex("^(complex\\s|complexo|complex$)",
            ignore_case = TRUE))
    
    complex <- stringr::str_detect(
      spp_names,
      regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
            ignore_case = TRUE))
    
    # genus novum | genus species
    gen0 <- stringr::str_detect(
      spp_names,
      regex("^(gen\\.|gen\\s|gen$)",
            ignore_case = TRUE))
    
    gen <- stringr::str_detect(
      spp_names,
      regex("\\sgen\\.|\\sgen\\s|\\sgen$",
            ignore_case = TRUE))
    
    
    # species | species (plural)
    sp0 <- stringr::str_detect(
      spp_names,
      regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$|sp[[:digit:]]|spp[[:digit:]])",
            ignore_case = TRUE))
    
    sp <- stringr::str_detect(
      spp_names,
      regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$|\\ssp[[:digit:]]|\\sspp[[:digit:]]|\\sssp[[:digit:]]",
            ignore_case = TRUE))
    
    # species incerta
    sp_inc0 <- stringr::str_detect(
      spp_names,
      regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
            ignore_case = TRUE))
    
    sp_inc <- stringr::str_detect(
      spp_names,
      regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
            ignore_case = TRUE))
    
    # species inquirenda
    sp_inq0 <- stringr::str_detect(
      spp_names,
      regex("^(inq\\.|inq\\s|inq$)",
            ignore_case = TRUE))
    
    sp_inq <- stringr::str_detect(
      spp_names,
      regex("\\sinq\\.|\\sinq\\s|\\sinq$",
            ignore_case = TRUE))
    
    # species indeterminabilis
    sp_indet0 <- stringr::str_detect(
      spp_names,
      regex(
        "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
        ignore_case = TRUE))
    
    sp_indet <- stringr::str_detect(
      spp_names,
      regex(
        "\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
        ignore_case = TRUE))
    
    # species nova
    sp_nova0 <- stringr::str_detect(
      spp_names,
      regex("^(nov\\.|nov\\s|nov$)",
            ignore_case = TRUE))
    
    sp_nova <- stringr::str_detect(
      spp_names,
      regex("\\snov\\.|\\snov\\s|\\snov$",
            ignore_case = TRUE))
    
    # species proxima
    sp_proxima0 <- stringr::str_detect(
      spp_names,
      regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
            ignore_case = TRUE))
    
    sp_proxima <- stringr::str_detect(
      spp_names,
      regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
            ignore_case = TRUE))
    
    # stetit
    stet0 <- stringr::str_detect(
      spp_names,
      regex("^(stet\\.|stet\\s|stet$)",
            ignore_case = TRUE))
    
    stet <- stringr::str_detect(
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
    tab_res <- as.data.frame(cbind(taxo_uncertainty, term_uncertainty))
    w <- which(is.na(spp_names))
    tab_res[w, "taxo_uncertainty"] <- NA
    
    colnames(tab_res) <- c(".uncer_terms", "uncer_terms")
    df <- dplyr::bind_cols(data, tab_res)
    
    
    
    
    ### Remove uncertainty terms
    
    # confer
    spp_names_clean <-
      stringr::str_replace_all(
        spp_names,
        regex("^(cf\\.|cf\\s|cf$)",
              ignore_case = TRUE),
        replacement = " ") # at the beginning
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\scf\\.|\\scf\\s|\\scf$",
            ignore_case = TRUE),
      replacement = " ") # anywhere
    
    # affinis
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(aff\\.|aff\\s|aff$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\saff\\.|\\saff\\s|\\saff$",
            ignore_case = TRUE),
      replacement = " ")
    
    # complex
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(complex\\s|complexo|complex$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
            ignore_case = TRUE),
      replacement = " ")
    
    # genus novum | genus species
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(gen\\.|gen\\s|gen$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sgen\\.|\\sgen\\s|\\sgen$",
            ignore_case = TRUE),
      replacement = " ")
    
    # species | species (plural)
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$|sp[[:digit:]]|spp[[:digit:]])",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$|\\ssp[[:digit:]]|\\sspp[[:digit:]]|\\sssp[[:digit:]]",
            ignore_case = TRUE),
      replacement = " ")
    
    # species incerta
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
            ignore_case = TRUE),
      replacement = " ")
    
    # species inquirenda
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(inq\\.|inq\\s|inq$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sinq\\.|\\sinq\\s|\\sinq$",
            ignore_case = TRUE),
      replacement = " ")
    
    # species indeterminabilis
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex(
        "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
        ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
            ignore_case = TRUE),
      replacement = " ")
    
    # species nova
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(nov\\.|nov\\s|nov$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\snov\\.|\\snov\\s|\\snov$",
            ignore_case = TRUE),
      replacement = " ")
    
    # species proxima
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
            ignore_case = TRUE),
      replacement = " ")
    
    # stetit
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(stet\\.|stet\\s|stet$)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sstet\\.|\\sstet\\s|\\sstet$",
            ignore_case = TRUE),
      replacement = " ")
    
    # hybrids
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(x\\s)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sx\\s|\\sx$",
            ignore_case = TRUE),
      replacement = " ")
    
    
    # Non-available (NA)
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\sna\\.|\\sna\\s|\\sna$",
            ignore_case = TRUE),
      replacement = " ")
    
    # sem
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("^(sem\\s|sem\\.)",
            ignore_case = TRUE),
      replacement = " ")
    
    spp_names_clean <- stringr::str_replace_all(
      spp_names_clean,
      regex("\\ssem\\.|\\ssem\\s|\\ssem$",
            ignore_case = TRUE),
      replacement = " ")
    
    # Remove extra spaces
    spp_names_clean <- 
      stringr::str_squish(spp_names_clean) %>% 
      as_tibble %>%
      dplyr::rename(clean_uncer_terms = value)
    
    # Add column to the database
    df <- dplyr::bind_cols(df, spp_names_clean)
    
    message(
      paste(
        "bdc_rem_taxo_unc:\nRemoved and flagged",
        sum(!taxo_uncertainty),
        "records.\nThree collumns were added to the database.\n"))
    
    return(df)
  }
  
# Other issues ------------------------------------------------------------
   # Capitalizes genus name and strings in which all words are capitalized; substitute empty cells with NA  
  bdc_rem_other_issues <- function(data, sci_names) {
    
    res <- data[[sci_names]] %>% stringr::str_squish()
    
    # count the number of words
    word_count <- stringr::str_count(res, "\\w+")
    
    # Convert to lower case and capitalize the only first letter of the generic names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae)
    w1 <- which(word_count == 1)
    res[w1] <- stringr::str_to_lower(res[w1])
    res[w1] <- Hmisc::capitalize(res[w1])
    
    res <-
      gsub("^$", NA, res) %>% # substitute empty records by NA
      Hmisc::capitalize(.) # Capitalize first letter
    
    for (i in 1:length(res)) {
      all_capitalize <- !stringr::str_detect(res[i], "[[:lower:]]")
      
      if (all_capitalize == TRUE) {
        res[i] <- stringr::str_to_lower(res[i]) %>% Hmisc::capitalize(.) 
      }
    }
    
    df <-
      data.frame(res) %>%
      rename(clean_other_issues = res) %>%
      dplyr::bind_cols(data, .)
    
    message(
      paste(
        "bdc_rem_other_issues:\nOne collumns was added to the database.\n"
      )
    )
    
    return(df)
  }
# Infraespecific names ----------------------------------------------------
  # Flag, identify and remove infraespecific categories from scientific names  
  bdc_rem_infaesp_names <- function(data, sci_names) {
    
    # Note that:
    # \\s = space
    # \\. = end point
    # | = or
    # $ = search at the end of a string
    
    ### Flag infraespecific terms (variety, subspecies, forma)
    
    sci_names <- data[[sci_names]] %>% stringr::str_squish()
    
    # subspecies
    subsp0 <- stringr::str_detect(
      sci_names,
      regex(
        "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
        ignore_case = TRUE
      )
    )
    
    subsp <- stringr::str_detect(
      sci_names,
      regex(
        "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
        ignore_case = TRUE
      )
    )
    
    # forma
    forma <- stringr::str_detect(
      sci_names, "\\sf\\.\\s|\\sf\\s|\\sfo\\.\\s|\\sfo\\s") &
      !stringr::str_detect(sci_names, "\\sf.\\s&|\\sf.\\sex")
    
    # varietas (variety)
    var <- stringr::str_detect(
      sci_names,
      regex(
        "\\svar\\.|\\svar\\s|\\svar$|\\ssubvar\\.|\\ssubvar\\s|\\ssubvar$",
        ignore_case = TRUE
      )
    )
    
    terms <- c("subsp0", "subsp", "forma", "var")
    terms_names <- c("subspecies", "subspecies", "forma", "varietas")
    
    infraesp_term <- rep(NA, length(sci_names))
    .infraesp_names <- rep(FALSE, length(sci_names))
    
    for (i in 1:length(terms)) {
      t <- get(terms[i])
      posi <- which(t == TRUE)
      infraesp_term[posi] <- terms_names[i]
      .infraesp_names[posi] <- TRUE
    }
    
    .infraesp_names <- ifelse(.infraesp_names == TRUE, FALSE, TRUE)
    tab_res <- as.data.frame(cbind(.infraesp_names, infraesp_term))
    w <- which(is.na(sci_names))
    tab_res[w, ".infraesp_names"] <- NA
    
    # df <- dplyr::bind_cols(data, tab_res)
    
    
    ### Remove infraespecif names
    spp_names_clean <- sci_names
    posi <- which(.infraesp_names == FALSE)
    
    # subspecies
    rem_infra <- stringr::str_replace_all(
      spp_names_clean[posi],
      regex(
        "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
        ignore_case = TRUE
      ),
      replacement = " "
    )
    
    rem_infra <- stringr::str_replace_all(
      rem_infra,
      regex(
        "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
        ignore_case = TRUE
      ),
      replacement = " "
    )
    
    
    # forma
    rem_infra <- stringr::str_replace_all(
      rem_infra,
      regex("\\sf\\.\\s|\\sf\\s|\\sfo\\.\\s|\\sfo\\s",
            ignore_case = FALSE
      ),
      replacement = " "
    )
    
    # varietas (variety)
    rem_infra <- stringr::str_replace_all(
      rem_infra,
      regex(
        "\\svar\\.|\\svar\\s|\\svar$|\\ssubvar\\.|\\ssubvar\\s|\\ssubvar$",
        ignore_case = TRUE
      ),
      replacement = " "
    )
    
    spp_names_clean[posi] <- rem_infra
    
    # Remove extra spaces
    clean_infaesp_names <-
      stringr::str_squish(spp_names_clean) %>%
      as_tibble() %>%
      dplyr::rename(clean_infaesp_names = value)
    
    # Add column to the database
    df <- dplyr::bind_cols(data, tab_res, clean_infaesp_names)
    
    message(
      paste(
        "bdc_rem_infraesp_names:\nRemoved and flagged",
        sum(!.infraesp_names),
        "records.\nThree collumns were added to the database.\n"
      )
    )
    return(df)
  }
  
# Gnparser ----------------------------------------------------------------
  # Parse scientific names using rgnparser package. For more details, see https://ropensci.org/technotes/2020/08/25/scientific-name-parsing/
  bdc_gnparser <- function(data, sci_names) {
    
    # one-time setup to download and install rgnparser, which is used to parse scientific name (for more details, see https://github.com/ropensci/rgnparser)
    rgnparser::install_gnparser(force = F)
    
    data_temp <- data
    w <- which(colnames(data_temp) == sci_names)
    colnames(data_temp)[w] <- "temp"
    data_temp$id <- 1:nrow(data_temp)
    
    # Parse names using rgnparser
    suppressWarnings({
      gnparser <-
        data_temp %>%
        pull(temp) %>%
        rgnparser::gn_parse_tidy() %>%
        select(canonicalfull, cardinality, quality, verbatim) %>%
        rename(
          names_clean = canonicalfull,
          temp = verbatim
        )
    })
    
    # Add names parsed to the full database
    df <-
      full_join(data_temp, gnparser, by = "temp") %>%
      distinct(id, .keep_all = T) %>%
      select(-c(id, temp))
    
    message(
      paste(
        "\nbdc_gnparser:\n",
        "Three collumns were added to the database.\n"
      )
    )
    
    return(df)
  }
  
  # Parse names
  parse_names <- 
    bdc_rem_family_names(data = names, sci_names = "scientificName") %>% 
    bdc_rem_taxo_unc(data = ., sci_names = "clean_family_names") %>% 
    bdc_rem_other_issues(data = ., sci_names = "clean_uncer_terms")  %>% 
    bdc_rem_infaesp_names(data = ., sci_names = "clean_other_issues") %>% 
    bdc_gnparser(data = ., sci_names = "clean_infaesp_names")
  
  parse_names <-
    parse_names %>%
    dplyr::select(
      scientificName,
      .uncer_terms,
      uncer_terms,
      .infraesp_names,
      names_clean,
      cardinality,
      quality
    )
  
  # Join results to data
  df_join <- dplyr::full_join(names_raw, parse_names, by = "scientificName")
  
  return(df_join)
}