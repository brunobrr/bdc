ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}


# Transformation of coordinates
coord_trans <- function(data, x, y, country_code, id, worldmap, worldmap_cntr_code) {
  
  data <- data %>% dplyr::select(x, y, country_code, id)
  d1 <- data.frame(x = data[, x], y = -data[, y])
  d2 <- data.frame(x = -data[, x], y = data[, y])
  d3 <- data.frame(x = -data[, x], y = -data[, y])
  d4 <- data.frame(x = data[, y], y = data[, x])
  d5 <- data.frame(x = data[, y], y = -data[, x])
  d6 <- data.frame(x = -data[, y], y = data[, x])
  d7 <- data.frame(x = -data[, y], y = -data[, x])
  
  d.list <- list(d1, d2, d3, d4, d5, d6, d7)
  rm(list = paste0('d', 1:7))
  d.list <- lapply(d.list, function(x) {
    colnames(x) <- c('x', 'y')
    return(x)
  })
  
  over_list <- list()
  
  for (d in 1:length(d.list)) {
    caluse <- sp::SpatialPoints(d.list[[d]])
    caluse@proj4string <- worldmap@proj4string
    overresult <- sp::over(caluse, worldmap)
    colnames(d.list[[d]]) <- c(paste0(x, '_modified'), paste0(y, '_modified'))
    over_list[[d]] <- data.frame(d.list[[d]], data, overresult)
    rm(caluse)
    filt <-
      which(over_list[[d]][country_code] == over_list[[d]][worldmap_cntr_code])
    if (length(filt) > 0) {
      over_list[[d]] <- over_list[[d]][filt, ]
    } else {
      over_list[[d]] <- NULL
    }
    rm(list = c('caluse', 'overresult', 'filt'))
  }
  
  rm(d.list)
  
  over_list <- over_list[!sapply(over_list <- over_list, is.null)]
  over_list <- dplyr::bind_rows(over_list)
  return(over_list)
}
