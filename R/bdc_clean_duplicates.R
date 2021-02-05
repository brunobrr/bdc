#' Title: Filter duplicated names in bdc_get_taxa_taxadb function.
#' Function only to internal use of bdc_get_taxa_taxadb function.
#' 
#' @param data A data.frame created in bdc_get_taxa_taxadb.  
#'
#' @return Return the data.frame without duplicates. 
#' @export
#'
#' @examples
bdc_clean_duplicates <- function(data) {
  data <- data[order(data$taxonomicStatus), ]
  data <- data[!(duplicated(data$input) & data$taxonomicStatus != "accepted"), ] 
  valid_duplicates <- data[duplicated(data$input) & data$taxonomicStatus == "accepted", "scientificName"]
  data <- data[!duplicated(data$input), ]

  if (length(valid_duplicates[[1]]) > 0) {
    for (i in unique(valid_duplicates$scientificName)) {
      index <- grep(i, data$scientificName)
      data[index, "notes"] <- "|check +1 accepted"
      data$input[index] <- i
    }
  }

  return(data)
}
