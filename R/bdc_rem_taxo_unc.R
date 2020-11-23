
#' Title: Remove terms denoting taxonomic uncertainty (e.g., cf., sp., aff)
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_taxo_unc <- function(spp_names) {
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

