return_names <- function(sci_name, max.distance, species.first.letter){

  out <- stringdist::stringdist(sci_name, species.first.letter)
  #p_max <- pmax(nchar(sci_name), nchar(species.first.letter$scientificName))
  #distance <- 1 - (out/p_max)
  #max.dist <- max(distance, na.rm = TRUE)
#  min_dist_name <- species.first.letter[out == min(out)][1]
  min_dist_name <- species.first.letter[out == sort(out, decreasing = FALSE)[1]][1]
  sorted <- sort(c(nchar(sci_name), nchar(min_dist_name)))
  
  if (max.dist >= max.distance) {
    
    return(data.frame(suggested = min_dist_name, distance = max.dist))
    
  }
  else {
    
    return(data.frame(suggested = NA, distance = max.dist))
    
  }
}





