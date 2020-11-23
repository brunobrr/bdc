

# bdc_round_dec -----------------------------------------------------------

#' Title: Function to test round coordinates
#'
#' @param data: data.frame. A data.frame with coordinates data
#' @param lon: character. Column names with longitude values
#' @param lat: character. Column names with latitude values
#' @param ndec: ndec: numeric. A vector with number of decimal to be tested. Default ndec=c(0,1,2) 
#'
#' @return
#' @export
#'
#' @examples
bdc_round_dec <-
  function(data,
           lon = "decimalLongitude",
           lat = "decimalLatitude",
           ndec = c(0, 1, 2)) {
    data <-
      data[, c(lon, lat)] %>%
      as.data.frame()
    
    ndec_lat <- (data[, lat] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    
    ndec_lon <- (data[, lon] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    
    rm(data)
    
    ndec_list <- as.list(ndec)
    names(ndec_list) <- paste0(".", "ndec", ndec)
    
    for (i in 1:length(ndec)) {
      message("Testing coordinate with ", ndec[i], " decimal")
      ndec_list[[i]] <- !(ndec_lat == ndec[i] & ndec_lon == ndec[i])
      message("Flagged ", sum(!ndec_list[[i]]), " records")
    }
    ndec_list <- dplyr::bind_cols(ndec_list)
    ndec_list$.ndec_all <- apply(ndec_list, 1, all) # all flagged as low decimal precision
    return(ndec_list)
  }

