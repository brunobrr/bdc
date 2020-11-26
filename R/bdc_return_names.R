return_names <- function(max.distance, max.dist, distance, species.first.letter, uncertain, ident, return.na){
if (max.dist >= max.distance) {
  
  if (length(ident) == 0L) {
    dis <- max(distance, na.rm = TRUE)
    res <- species.first.letter$scientificName[distance == dis][1]
    
    if (length(uncertain) == 0L) {
      
      return(c(res, dis))
    }
    else {
      res <- unlist(strsplit(res, " "))
      return(c(paste(res[1], uncertain, res[2:length(res)]), dis))
    }
  }
  else {
    paste(species.first.letter$scientificName[distance == max(distance, 
                                                              na.rm = TRUE)][1], ident, sep = "")
  }
}
else {
  if (return.na) {
    c(NA, "0") # Mudar o valor de 0 para o real
  }
  else {
    taxon.orig
  }
}
}





