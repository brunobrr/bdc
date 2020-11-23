
#' Title: Remove family names from species names
#'
#' @param sp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_family_names <- function(sp_names) {
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
