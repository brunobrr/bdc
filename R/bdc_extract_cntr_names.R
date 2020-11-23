
#' Title: Extract_cntr_names is a function used to extracting country names in different names from wikipedia in different languages
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
bdc_extract_cntr_names <- function(x) {
  if (stringr::str_detect(x, "Note")) {
    x <- stringr::str_split(x, "Note")[[1]][1]
  }
  if (stringr::str_detect(x, "[*]")) {
    x <- stringr::str_split(x, "[*]")[[1]][1]
  }
  if (stringr::str_detect(x, "Alternate, older forms")) {
    x <- stringr::str_split(x, "Alternate, older forms")[[1]][1]
  }
  x <-
    stringr::str_split(x, pattern = "[)]")[[1]] %>%
    stringr::str_split_fixed(., pattern = "[(]", n = 2)
  x <- x[, 1]
  x <-
    x %>%
    stringr::str_split(., pattern = ", ") %>%
    unlist() %>%
    stringr::str_split(., pattern = " ,") %>%
    unlist() %>%
    stringr::str_split(., pattern = ",") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE)
  
  x2 <- x[!str_detect(x, "/|-")]
  
  x3.1 <- x[str_detect(x, "/")] %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim()
  x3.2 <- x[str_detect(x, "-")] %>%
    stringr::str_split(., pattern = " -", n = 2) %>%
    unlist() %>%
    stringr::str_trim()
  
  x <- c(x2, x3.1, x3.2) %>%
    sort() %>%
    stringr::str_split(., pattern = "/") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " -") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "- ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = " or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "or ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = ". ") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_split(., pattern = "[\n]") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_subset(., pattern = "", negate = FALSE) %>%
    sort() %>%
    unique()
  
  x <- x[!(str_length(x) == 1 & grepl(".", x))]
  if (any(x == "Afghanistan")) {
    x <- x[-1]
  }
  x <- x %>%
    data.frame() %>%
    as_tibble()
  
  return(x) # Country name in different language
}

