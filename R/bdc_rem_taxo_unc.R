
#' Flag, identify and remove terms denoting uncertainty or provisional status of a taxonomic identification
#'
#' @param data 
#' @param sci_names 
#'
#' @return
#' @export
#'
#' @examples
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

