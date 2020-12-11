
#' Title: Remove duplicated genus, substitute empty cells by NA, capitalize generic name
#'
#' @param spp_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_other_issues <- function(spp_names) {
  
  res <- spp_names
  word_count <- stringr::str_count(spp_names, "\\w+")
  
  w1 <- which(word_count == 1 | word_count == 2)
  res[w1] <- stringr::str_to_lower(res[w1])
  res[w1] <- Hmisc::capitalize(res[w1])
  
  w3 <- which(word_count >= 3)
  
  for (i in w3)
  {
    sp <- tolower(spp_names[i])
    sp <- gsub(pattern = "-", " ", sp)
    t <- trimws(sp)
    u <- unlist(
      strsplit(t, split = " ", fixed = F, perl = T)
    )
    dup <- paste(unique(c(u[1], u[2])), sep = " ", collapse = " ")
    remain <- paste(u[3:length(u)], sep = " ", collapse = " ")
    p <- paste(dup, remain)
    res[i] <- p
  }
  v3 <- gsub("^$", NA, res) # substitue empty records by NA
  v4 <- Hmisc::capitalize(v3)
  return(v4)
}
