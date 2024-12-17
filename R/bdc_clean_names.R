bin_on_path <- function() grepl("gnparser", Sys.getenv("PATH"))
bin_exec <- function() Sys.which("gnparser") != ""
exec_exists <- function(path) file.exists(path)
is_windows <- function() .Platform$OS.type == "windows"
is_macos <- function() unname(Sys.info()["sysname"] == "Darwin")
is_linux <- function() unname(Sys.info()["sysname"] == "Linux")

test_gnparser_setup <- function(bin_full_path){
  
  if (!exec_exists(bin_full_path)) message("GNparser is not installed in your machine.")
}

check_gnparser_setup <- function(){
  
  if (is_windows() && !bin_on_path() && !bin_exec()) {
      test_gnparser_setup(paste0(Sys.getenv("APPDATA"), "\\gnparser\\gnparser.exe"))
    } else if (is_macos() && !bin_on_path() && !bin_exec()) {
      test_gnparser_setup(normalizePath("~/Library/Application Support/gnparser"))
    } else if (is_linux() && !bin_on_path() && !bin_exec()) {
      test_gnparser_setup(normalizePath("~/bin/gnparser"))
    }
  
}


#' Clean and parse scientific names
#'
#' This function is composed of a series of name-checking routines for cleaning
#' and parsing scientific names; i.e., unify writing style. It removes 1) family
#' names of animals or plants pre-pended to species names, 2) qualifiers
#' denoting the uncertain or provisional status of taxonomic identification
#' (e.g., confer, species, affinis), and 3) infraspecific terms, for example,
#' variety (var.), subspecies (subsp), forma (f.), and their spelling
#' variations. It also includes applications to 4) standardize names, i.e.,
#' capitalize only the first letter of the genus name and remove extra
#' whitespaces), and 5) parse names, i.e., separate author, date, annotations
#' from taxon name.
#' 

#'
#' @family taxonomy
#' @param sci_names character string. Containing scientific names.
#' @param save_outputs logical. Should the outputs be saved? Default = FALSE.
#'
#' @details Terms denoting uncertainty or provisional status of taxonomic
#' identification as well as infraspecific terms were obtained from Sigoviniet
#' al. (2016; doi: 10.1111/2041-210X.12594).
#'
#' @return A five-column data.frame including
#' * scientificName: original names supplied
#' * .uncer_terms: indicates the presence of taxonomic uncertainty terms
#' * .infraesp_names: indicates the presence of infraspecific terms
#' * name_clean: scientific names resulting from the cleaning and parsing
#' processes
#' * quality: an index indicating the quality of parsing process. It
#' ranges from 0 to 4, being 1 no problem detected, 4 serious problems detected;
#' a value of 0 indicates no interpretable name that was not parsed).
#'
#' If save_outputs == TRUE, a data.frame containing all tests of the cleaning
#' names process and the results of the parsing names process is saved in
#' "Output/Check/02_parse_names.csv".
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr rename distinct select mutate_all na_if filter tibble pull
#' bind_cols full_join
#' @importFrom here here
#' @importFrom rgnparser gn_parse_tidy
#' @importFrom stringr str_squish str_count str_remove_all regex str_detect
#' str_which str_replace_all str_to_lower
#' @importFrom tibble as_tibble
#' @importFrom stringi stri_trans_general
#'
#' @export
#'
#' @examples
#' \dontrun{
#' scientificName <- c(
#'   "Fridericia bahiensis (Schauer ex. DC.) L.G.Lohmann",
#'   "Peltophorum dubium (Spreng.) Taub. (Griseb.) Barneby",
#'   "Gymnanthes edwalliana (Pax & K.Hoffm.) Laurenio-Melo & M.F.Sales",
#'   "LEGUMINOSAE Senna aff. organensis (Glaz. ex Harms) H.S.Irwin & Barneby"
#' )
#'
#' bdc_clean_names(scientificName, save_outputs = FALSE)
#' }
#' 
bdc_clean_names <- function(sci_names, save_outputs = FALSE) {
  value <- scientificName <- X1 <- value <- . <- temp <- canonicalfull <- NULL
  cardinality <- quality <- verbatim <- id <- . <- .uncer_terms <- . <- NULL
  .infraesp_names <- names_clean <- NULL

  # Chech if gnparser is installed. Otherwise, guide user about installation.
  check_gnparser_setup()
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
    dplyr::mutate_all(dplyr::na_if, "") %>% # change empty names to NA
    dplyr::filter(!is.na(scientificName)) # remove NAs

  # Parse names
  parse_names <-
    bdc_rem_family_names(data = names, sci_names = "scientificName") %>%
    bdc_rem_taxo_unc(data = ., sci_names = "clean_family_names") %>%
    bdc_rem_other_issues(data = ., sci_names = "clean_uncer_terms") %>%
    bdc_rem_infaesp_names(data = ., sci_names = "clean_other_issues") %>%
    bdc_gnparser(data = ., sci_names = "clean_infaesp_names")

  # Join results to data
  df_join <- dplyr::full_join(names_raw, parse_names, by = "scientificName")

  if (save_outputs == TRUE) {
    bdc_create_dir()

    # Save the results of the parsing names process
    readr::write_csv(df_join,
                     here::here("Output", "Check", "02_parsed_names.csv"))

    message(
      paste(
        ">> Scientific names were cleaned and parsed. Check the results in 'Output/Check/02_clean_names.csv'.\n"
      )
    )
  }

  # Return a "clean" database
  df_join <-
    df_join %>%
    dplyr::select(
      scientificName,
      .uncer_terms,
      .infraesp_names,
      names_clean,
      quality
    )

  return(df_join)
}


# Function for capitalizing only first letter -----------------------------
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Family names -----------------------------------------------------------
bdc_rem_family_names <- function(data, sci_names) {
  X1 <- NULL

  # Get animalia family names from gbif via taxadb package
  animalia_families <-
    system.file("extdata", "family_names/animalia_families.txt",
      package = "bdc"
    ) %>%
    readr::read_csv(col_names = FALSE, show_col_types = FALSE) %>%
    dplyr::tibble() %>%
    dplyr::pull(X1)

  # Raw scientific names
  sci_names_raw <- data[[sci_names]] %>% stringr::str_squish()

  # Vector to save scientific names without family names
  clean_family_names <- sci_names_raw

  # Count number of words to avoid removing names composed of a single family
  # name
  word_count <- stringr::str_count(sci_names_raw, "\\S+")
  df <- data.frame(word_count, sci_names_raw)
  n_string <- ifelse(df[, 1] < 2, TRUE, FALSE)
  n_string <- ifelse(is.na(n_string), TRUE, n_string)

  posi <- which(n_string == FALSE)

  # Plantae: remove suffix "aceae"
  rem_fam <- stringr::str_remove_all(
    sci_names_raw[posi],
    stringr::regex("[a-zA-Z]+aceae|Leguminosae|Compositae",
      ignore_case = TRUE
    )
  ) %>%
    stringr::str_squish()

  clean_family_names[posi] <- rem_fam
  .family_names <- sci_names_raw == clean_family_names


  # Animalia: detect suffix "dae"
  detect_fam_animalia <- stringr::str_detect(
    sci_names_raw[posi],
    stringr::regex("[a-zA-Z]+dae", ignore_case = TRUE),
    negate = T
  )

  df <-
    data.frame(posi, detect_fam_animalia, sci_names_raw[posi]) %>%
    dplyr::filter(detect_fam_animalia == FALSE)

  # Compare with a list of known taxonomic family to avoid erroneously
  # flagging generic names (e.g. "Solanum lacerdae")
  check <- df$posi
  is_valid <- NULL
  for (i in check) {
    test <- stringr::str_which(
      sci_names_raw[i],
      stringr::regex(animalia_families, ignore_case = T)
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
      stringr::regex("[a-zA-Z]+dae",
        ignore_case = TRUE
      )
    ) %>%
      stringr::str_squish()

    clean_family_names[posi_valid] <- rem_fam_animalia
  }

  .family_names <- sci_names_raw == clean_family_names
  .family_names <- ifelse(is.na(.family_names), TRUE, .family_names)

  df_final <- data.frame(.family_names, clean_family_names)
  df_final <- dplyr::bind_cols(data, df_final)

  message(
    paste(
      "\n>> Family names prepended to scientific names were flagged and removed from",
      sum(!.family_names),
      "records."
    )
  )

  return(df_final)
}

# Uncertainty terms -------------------------------------------------------
# Flag, identify and remove terms denoting uncertainty or provisional status
# of a taxonomic identification
bdc_rem_taxo_unc <- function(data, sci_names) {
  value <- NULL
  spp_names <- data[[sci_names]] %>% stringr::str_squish()

  ### Flag and identify uncertainty terms

  # confer
  cf0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(cf\\.|cf\\s|cf$)",
      ignore_case = TRUE
    )
  ) # at the beginning

  cf <- stringr::str_detect(
    spp_names,
    stringr::regex("\\scf\\.|\\scf\\s|\\scf$",
      ignore_case = TRUE
    )
  ) # anywhere

  # affinis
  aff0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(aff\\.|aff\\s|aff$)",
      ignore_case = TRUE
    )
  )

  aff <- stringr::str_detect(
    spp_names,
    stringr::regex("\\saff\\.|\\saff\\s|\\saff$",
      ignore_case = TRUE
    )
  )

  # complex
  complex0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(complex\\s|complexo|complex$)",
      ignore_case = TRUE
    )
  )

  complex <- stringr::str_detect(
    spp_names,
    stringr::regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
      ignore_case = TRUE
    )
  )

  # genus novum | genus species
  gen0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(gen\\.|gen\\s|gen$)",
      ignore_case = TRUE
    )
  )

  gen <- stringr::str_detect(
    spp_names,
    stringr::regex("\\sgen\\.|\\sgen\\s|\\sgen$",
      ignore_case = TRUE
    )
  )

  # species | species (plural)
  sp0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$|sp[[:digit:]]|spp[[:digit:]])",
      ignore_case = TRUE
    )
  )

  sp <- stringr::str_detect(
    spp_names,
    stringr::regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$|\\ssp[[:digit:]]|\\sspp[[:digit:]]|\\sssp[[:digit:]]",
      ignore_case = TRUE
    )
  )

  # species incerta
  sp_inc0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
      ignore_case = TRUE
    )
  )

  sp_inc <- stringr::str_detect(
    spp_names,
    stringr::regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
      ignore_case = TRUE
    )
  )

  # species inquirenda
  sp_inq0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(inq\\.|inq\\s|inq$)",
      ignore_case = TRUE
    )
  )

  sp_inq <- stringr::str_detect(
    spp_names,
    stringr::regex("\\sinq\\.|\\sinq\\s|\\sinq$",
      ignore_case = TRUE
    )
  )

  # species indeterminabilis
  sp_indet0 <- stringr::str_detect(
    spp_names,
    stringr::regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE
    )
  )

  sp_indet <- stringr::str_detect(
    spp_names,
    stringr::regex(
      "\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
      ignore_case = TRUE
    )
  )

  # species nova
  sp_nova0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(nov\\.|nov\\s|nov$)",
      ignore_case = TRUE
    )
  )

  sp_nova <- stringr::str_detect(
    spp_names,
    stringr::regex("\\snov\\.|\\snov\\s|\\snov$",
      ignore_case = TRUE
    )
  )

  # species proxima
  sp_proxima0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
      ignore_case = TRUE
    )
  )

  sp_proxima <- stringr::str_detect(
    spp_names,
    stringr::regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
      ignore_case = TRUE
    )
  )

  # stetit
  stet0 <- stringr::str_detect(
    spp_names,
    stringr::regex("^(stet\\.|stet\\s|stet$)",
      ignore_case = TRUE
    )
  )

  stet <- stringr::str_detect(
    spp_names,
    stringr::regex("\\sstet\\.|\\sstet\\s|\\sstet$",
      ignore_case = TRUE
    )
  )

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
  taxo_uncertainty <- ifelse(is.na(taxo_uncertainty), TRUE, taxo_uncertainty)

  tab_res <- cbind.data.frame(taxo_uncertainty, term_uncertainty)
  w <- which(is.na(spp_names))
  tab_res[w, "taxo_uncertainty"] <- NA

  colnames(tab_res) <- c(".uncer_terms", "uncer_terms")
  df <- dplyr::bind_cols(data, tab_res)


  ### Remove uncertainty terms

  # confer
  spp_names_clean <-
    stringr::str_replace_all(
      spp_names,
      stringr::regex("^(cf\\.|cf\\s|cf$)",
        ignore_case = TRUE
      ),
      replacement = " "
    ) # at the beginning

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\scf\\.|\\scf\\s|\\scf$",
      ignore_case = TRUE
    ),
    replacement = " "
  ) # anywhere

  # affinis
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(aff\\.|aff\\s|aff$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\saff\\.|\\saff\\s|\\saff$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # complex
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(complex\\s|complexo|complex$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # genus novum | genus species
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(gen\\.|gen\\s|gen$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sgen\\.|\\sgen\\s|\\sgen$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species | species (plural)
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$|sp[[:digit:]]|spp[[:digit:]])",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$|\\ssp[[:digit:]]|\\sspp[[:digit:]]|\\sssp[[:digit:]]",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species incerta
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species inquirenda
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(inq\\.|inq\\s|inq$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sinq\\.|\\sinq\\s|\\sinq$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species indeterminabilis
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species nova
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(nov\\.|nov\\s|nov$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\snov\\.|\\snov\\s|\\snov$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # species proxima
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # stetit
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(stet\\.|stet\\s|stet$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sstet\\.|\\sstet\\s|\\sstet$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # hybrids
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(x\\s)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sx\\s|\\sx$",
      ignore_case = TRUE
    ),
    replacement = " "
  )


  # Non-available (NA)
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\sna\\.|\\sna\\s|\\sna$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # sem
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("^(sem\\s|sem\\.)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    stringr::regex("\\ssem\\.|\\ssem\\s|\\ssem$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # Remove extra spaces
  spp_names_clean <-
    stringr::str_squish(spp_names_clean) %>%
    tibble::as_tibble() %>%
    dplyr::rename(clean_uncer_terms = value)

  # Add column to the database
  df <- dplyr::bind_cols(df, spp_names_clean)

  message(
    paste(
      ">> Terms denoting taxonomic uncertainty were flagged and removed from",
      sum(!taxo_uncertainty),
      "records."
    )
  )

  return(df)
}

# Other issues ------------------------------------------------------------
# Capitalizes genus name and strings in which all words are capitalized;
# substitute empty cells with NA
bdc_rem_other_issues <- function(data, sci_names) {
  . <- NULL
  # Raw scientific names
  sci_names_raw <- data[[sci_names]] %>% stringr::str_squish()

  res <- data[[sci_names]] %>% stringr::str_squish()

  # count the number of words
  word_count <- stringr::str_count(res, "\\w+")

  # Convert to lower case and capitalize the only first letter of the generic
  # names (POLYGONACEAE to Polygonaceae; polygonaceae to Polygonaceae)
  w1 <- which(word_count == 1)
  res[w1] <- stringr::str_to_lower(res[w1])
  res[w1] <- firstup(res[w1])

  res <-
    gsub("^$", NA, res) %>% # substitute empty records by NA
    firstup(.) # Capitalize first letter

  all_capitalize <- NULL
  for (i in 1:length(res)) {
    all_capitalize[i] <- !stringr::str_detect(res[i], "[[:lower:]]")
  }

  cap_words <- which(all_capitalize == TRUE)
  for (i in 1:length(cap_words)) {
    res[i] <-
      stringr::str_to_lower(res[i]) %>%
      firstup(.)
  }

  clean_other_issues <- res
  .other_issues <- sci_names_raw == clean_other_issues
  .other_issues <- ifelse(is.na(.other_issues), TRUE, .other_issues)
  df <- data.frame(data, .other_issues, clean_other_issues)

  message(
    paste0(
      ">> Other issues, capitalizing the first letter of the generic name, replacing empty names by NA, and removing extra spaces, were flagged and corrected or removed from ",
      sum(!.other_issues),
      " records."
    )
  )

  return(df)
}

# Infraespecific names ----------------------------------------------------
# Flag, identify and remove infraespecific categories from scientific names
bdc_rem_infaesp_names <- function(data, sci_names) {
  value <- NULL

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
    stringr::regex(
      "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
      ignore_case = TRUE
    )
  )

  subsp <- stringr::str_detect(
    sci_names,
    stringr::regex(
      "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
      ignore_case = TRUE
    )
  )

  # forma
  forma <- stringr::str_detect(
    sci_names, "\\sf\\.\\s|\\sf\\s|\\sfo\\.\\s|\\sfo\\s"
  ) &
    !stringr::str_detect(sci_names, "\\sf.\\s&|\\sf.\\sex")

  # varietas (variety)
  var <- stringr::str_detect(
    sci_names,
    stringr::regex(
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
  .infraesp_names <- ifelse(is.na(.infraesp_names), TRUE, .infraesp_names)

  tab_res <- cbind.data.frame(.infraesp_names, infraesp_term)
  w <- which(is.na(sci_names))
  tab_res[w, ".infraesp_names"] <- NA

  # df <- dplyr::bind_cols(data, tab_res)


  ### Remove infraespecif names
  spp_names_clean <- sci_names
  posi <- which(.infraesp_names == FALSE)

  # subspecies
  rem_infra <- stringr::str_replace_all(
    spp_names_clean[posi],
    stringr::regex(
      "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  rem_infra <- stringr::str_replace_all(
    rem_infra,
    stringr::regex(
      "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
      ignore_case = TRUE
    ),
    replacement = " "
  )


  # forma
  rem_infra <- stringr::str_replace_all(
    rem_infra,
    stringr::regex("\\sf\\.\\s|\\sf\\s|\\sfo\\.\\s|\\sfo\\s",
      ignore_case = FALSE
    ),
    replacement = " "
  )

  # varietas (variety)
  rem_infra <- stringr::str_replace_all(
    rem_infra,
    stringr::regex(
      "\\svar\\.|\\svar\\s|\\svar$|\\ssubvar\\.|\\ssubvar\\s|\\ssubvar$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean[posi] <- rem_infra

  # Remove extra spaces
  clean_infaesp_names <-
    stringr::str_squish(spp_names_clean) %>%
    tibble::as_tibble() %>%
    dplyr::rename(clean_infaesp_names = value)

  # Add column to the database
  df <- dplyr::bind_cols(data, tab_res, clean_infaesp_names)

  message(
    paste(
      ">> Infraspecific terms were flagged and removed from",
      sum(!.infraesp_names),
      "records.\n"
    )
  )
  return(df)
}

# Gnparser ----------------------------------------------------------------
# Parse scientific names using rgnparser package. For more details,
# see https://ropensci.org/technotes/2020/08/25/scientific-name-parsing/
bdc_gnparser <- function(data, sci_names) {
  temp <- canonicalfull <- cardinality <- quality <- verbatim <- id <- scientificName <- NULL
  data_temp <- data
  w <- which(colnames(data_temp) == sci_names)
  colnames(data_temp)[w] <- "temp"
  data_temp$id <- 1:nrow(data_temp)
  data_temp$temp <-
    stringi::stri_trans_general(str = data_temp$temp, id = "Latin-ASCII") %>%
    stringr::str_squish()

  encod_chars <-
    readLines(system.file("extdata/encoding-chars.txt", package = "bdc"))

  first_group <- paste0("(", paste0(encod_chars[1:3], collapse = "|"), ")")
  second_group <- paste0("(", paste0(encod_chars[4:5], collapse = "|"), ")")

  data_temp$temp <- gsub(first_group, "", data_temp$temp)
  data_temp$temp <- gsub(second_group, " ", data_temp$temp)

  # Parse names using rgnparser
  suppressWarnings({
    suppressMessages({
      gnparser <-
        data_temp %>%
        dplyr::pull(temp) %>%
        rgnparser::gn_parse_tidy() %>%
        dplyr::select(canonicalfull, cardinality, quality, verbatim) %>%
        dplyr::rename(
          names_clean = canonicalfull,
          temp = verbatim
        )
    })
  })

  # Add names parsed to the full database
  df <-
    dplyr::left_join(data_temp, gnparser, by = "temp") %>%
    dplyr::distinct(id, .keep_all = T)

  s <- sum(is.na(df$scientificName))
  if (s > 0) {
    w <- df %>% dplyr::filter(is.na(scientificName))
    message(
      paste(
        "Names were not correctly parsed. Please, manually remove strange caracteres (accents, for example) from the following scientific names:\n"
      ),
      w$temp
    )

  }

  df <-
    df %>%
    dplyr::select(-c(id, temp))

  return(df)

}
