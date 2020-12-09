return_names <- function(taxon, max.distance, species.first.letter){

  out <- stringdist::stringdist(taxon, species.first.letter)
  #p_max <- pmax(nchar(taxon), nchar(species.first.letter$scientificName))
  #distance <- 1 - (out/p_max)
  #max.dist <- max(distance, na.rm = TRUE)
#  min_dist_name <- species.first.letter[out == min(out)][1]
  min_dist_name <- species.first.letter[out == sort(out, decreasing = FALSE)[1]][1]
  sorted <- sort(c(nchar(taxon), nchar(min_dist_name)))
  max.dist <- sorted[1]/sorted[2]
  
  if (max.dist >= max.distance) {
    
    return(data.frame(suggested = min_dist_name, distance = max.dist))
    
  }
  else {
    
    return(data.frame(suggested = NA, distance = max.dist))
    
  }
}





