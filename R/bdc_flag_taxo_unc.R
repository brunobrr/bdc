
#' Title: Flag terms denoting taxonomic uncertainty (T = no terms found; F = term found)
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_flag_taxo_unc <- function(spp_names) {
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

