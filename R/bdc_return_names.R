#' Title: Return the closest name in a vector of names. 
#' 
#' This function looks for the closest name in a vector of names and returns the string distances calculated by fuzzy matching.
#'
#' @param sci_name A character vector with a single name.
#' @param max.distance A numeric value specifying the minimum distance between the sci_name and the names in species.first.letter. 
#' @param species.first.letter A character vector whose distances will be calculated from sci_name.
#'
#' @return This function returns a data.frame whose first column is the closest name and the second column is the distance between the sci_name and the closest name.   
#' @export
#'
#' @examples
#' bdc_return_names("Cebus apela", max.distance = 0.75, species.first.letter = c("Cebus apella", "Puma concolar"))
#' 


bdc_return_names <- function(sci_name, max.distance, species.first.letter){

  out <- stringdist::stringdist(sci_name, species.first.letter)
  min_dist_name <- species.first.letter[out == sort(out, decreasing = FALSE)[1]][1]
  sorted <- sort(c(nchar(sci_name), nchar(min_dist_name)))
  max.dist <- 1- (min(out)/sorted[2])
  
  if (max.dist >= max.distance) {
    
    return(data.frame(suggested = min_dist_name, distance = round(max.dist, 2)))
    
  }
  else {
    
    return(data.frame(suggested = NA, distance = round(max.dist, 2)))
    
  }
}





