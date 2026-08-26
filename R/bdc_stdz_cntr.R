#' Internal function to match country names 
#'
#' @importFrom dplyr pull
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_replace_all str_trim
#'
#' @noRd
#' @return Return a vector with original corrected country names
#'
#' #' @examples
#' \dontrun{
#' }
bdc_stdz_cntr <- function (cntry_n, country_names_db, fuzzy_d = 1) 
{
  names_in_different_languages <- lower_case <-  english_name <- NULL
  
  cntr_names <- country_names_db
  
  if (any(is.na(cntry_n))) {
    cntry_n[which(is.na(cntry_n))] <- ""
  }
  cntry_n <- stringr::str_replace_all(cntry_n, "[[:punct:]]", " ") %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  f0 <- function(val0, cntr_names, fuzzy_d = fuzzy_d) {
    val <- tolower(val0)
    if(val=="" | is.na(val)){
      nms=NA
      return(nms)
    } else {
    nms <- cntr_names %>% dplyr::filter(
      dplyr::if_any(c(names_in_different_languages, lower_case), ~ .x%in%val)) %>% 
      dplyr::pull(english_name) %>% 
      unique()
    }
    
    if (length(nms) == 0) {
      d <- utils::adist(val, cntr_names %>% dplyr::pull(lower_case))
      mind <- which.min(d)
      if (d[mind] <= fuzzy_d) {
        nms <- cntr_names[mind,] %>% dplyr::pull(english_name)
      } else {
        d <- sapply(cntr_names$lower_case, function(x) utils::adist(val, x))
        d <- sapply(d, min)
        mind <- which.min(d)
        if (d[mind] <= fuzzy_d) {
          nms <- cntr_names[mind,] %>% dplyr::pull(english_name)
        }
      }
    }
    
    if(length(nms)>1){
      nms=NA
      return(nms)
    }
    # if (length(nms) == 0) {
    #   val <- val0
    #   d <- adist(val, cntr_names %>% dplyr::pull(names_in_different_languages ))
    #   mind <- which.min(d)
    #   if (d[mind] <= fuzzy_d) {
    #     nms <- cntr_names[mind,] %>% dplyr::pull(english_name)
    #   } else {
    #     d <- sapply(cntr_names$names_in_different_languages , function(x) adist(val, x))
    #     d <- sapply(d, min)
    #     mind <- which.min(d)
    #     if (d[mind] <= fuzzy_d) {
    #       nms <- cntr_names[mind,] %>% dplyr::pull(english_name)
    #     }
    #   }
    # }
    
    if (length(nms) == 0) {
      nms=NA
      return(nms)
    } else {
      return(nms)
    }
  }
  
  result <- sapply(cntry_n, function(x) f0(val0 = x, cntr_names = cntr_names, fuzzy_d = fuzzy_d))
  names(result) <- NULL
  result <- unlist(result)
  return(result)
}

#' Internal function to reword some country names
#'
#' @importFrom dplyr pull mutate recode
#' @importFrom purrr set_names
#' @importFrom readr read_delim
#'
#' @noRd
#' @return Return a tibble/sf object
#'
#' #' @examples
#' \dontrun{
#' }
bdc_reword_countries <- function(data) {

  after <- name_long <- NULL

  reword <-
    readr::read_delim(system.file("extdata/countries_names/reword-countries.txt", package = "bdc"), delim = "\t")

  vec_reword <-
    reword %>%
    dplyr::pull(after) %>%
    purrr::set_names(reword$before)

  data %>%
    dplyr::mutate(name_long = dplyr::recode(name_long, !!!vec_reword))

}

#' Internal function to map arbitrary country identifiers to a canonical name
#'
#' Resolves country identifiers — full English name, common name, or ISO
#' alpha-2/alpha-3 code — to the canonical (reworded) Natural Earth `name_long`
#' used throughout [bdc_coordinates_country_inconsistent()]. This makes name
#' comparisons robust to the multiple naming systems a database can contain
#' (e.g. "Russian Federation" vs "Russia", "United States" vs
#' "United States of America", or ISO codes such as "RU"/"RUS").
#'
#' @param worldmap sf/data.frame. A world map from
#'   `rnaturalearth::ne_countries(scale = "large", returnclass = "sf")` that has
#'   been reworded with `bdc_reword_countries()` and carries an extra
#'   `name_long_raw` column holding the original, non-reworded `name_long`.
#' @param x character. Country identifiers to resolve. NA values are returned as
#'   NA.
#'
#' @importFrom dplyr select all_of filter mutate
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_trim
#'
#' @noRd
#' @return A character vector of canonical country names aligned with `x` (NA
#'   when an identifier cannot be resolved).
bdc_canonical_country_name <- function(worldmap, x) {
  id_cols <- c("name_long", "name_long_raw", "name", "admin", "iso_a2", "iso_a3")

  d <- worldmap %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c("name_long", id_cols)))

  # Build a lookup table mapping every accepted identifier to its canonical name.
  # The first match wins, which avoids ambiguity for duplicate identifiers.
  lookup <- character()
  for (col in id_cols) {
    vals <- as.character(d[[col]])
    nms <- as.character(d[["name_long"]])
    keep <- !is.na(vals) & !(vals %in% c("-99", "NA", "N/A", ""))
    key <- tolower(stringr::str_trim(vals[keep]))
    nm <- nms[keep]
    add <- !(key %in% names(lookup))
    lookup[key[add]] <- nm[add]
  }

  vapply(x, function(one) {
    if (is.na(one)) {
      return(NA_character_)
    }
    key <- tolower(stringr::str_trim(as.character(one)))
    hit <- lookup[key]
    if (is.null(hit) || is.na(hit)) NA_character_ else unname(hit)
  }, character(1))
}
