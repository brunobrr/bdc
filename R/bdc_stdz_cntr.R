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
#' @importFrom readr read_csv
#'
#' @noRd
#' @return Return a tibble/sf object
#'
#' #' @examples
#' \dontrun{
#' }
bdc_reword_countries <- function(data) {

  after <- name_long <- NULL

  file_reword <-
    system.file("extdata/countries_names/reword-countries.csv", package = "bdc")

  reword <-
    readr::read_csv(file_reword, show_col_types = FALSE)

  vec_reword <-
    reword %>%
    dplyr::pull(after) %>%
    purrr::set_names(reword$before)

  data %>%
    dplyr::mutate(name_long = dplyr::recode(name_long, !!!vec_reword))

}
