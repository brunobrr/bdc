
# bdc_round_dec -----------------------------------------------------------

#' Title: Function to test round coordinates
#'
#' @param data: data.frame. A data.frame with coordinates data.
#' @param lon: character. Column names with longitude values.
#' @param lat: character. Column names with latitude values.
#' @param ndec: numeric. A vector with number of decimal to be tested. Default ndec=c(0,1,2). 
#'
#' @return The function returns a data.frame with logical values indicating whether values are rounded by the specified decimal number (ndec). 
#' Colnames indicates which decimal numbers were tested and the testes combined for all columns (.ndec_all)  
#' @export
#'
#' @examples
#' "data <- data.frame(lon = c(-55.389, -13.897, 30.678, 90.675) , lat = c(-21.345, 23.567, 16.798, -10.468))"
#' "bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = c(0, 2))"
#' 
bdc_round_dec <-
  function(data,
           lon = "decimalLongitude",
           lat = "decimalLatitude",
           ndec = c(0, 1, 2)) {
    df <-
      data %>% 
      dplyr::select({{lon}}, {{lat}}) %>% 
      as.data.frame()
    
    ndec_lat <- (df[, lat] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    
    ndec_lon <- (df[, lon] %>%
                   as.character() %>%
                   stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()
    
    rm(df)
    
    ndec_list <- as.list(ndec)
    names(ndec_list) <- paste0(".", "ndec", ndec)
    
    for (i in 1:length(ndec)) {
      ndec_list[[i]] <- !(ndec_lat == ndec[i] & ndec_lon == ndec[i])
    }
    ndec_list <- dplyr::bind_cols(ndec_list)
    ndec_list$.ndec_all <- apply(ndec_list, 1, all) # all flagged as low decimal precision
    
    ndec_list <-
      ndec_list %>% 
      dplyr::select(.ndec_all) %>%
      dplyr::rename(.rou = .ndec_all)
    
    message("bdc_round_dec:\nFlagged ", sum(!ndec_list[".rou"]), " records\nOne column was added to the database.\n")
    
   res <- dplyr::bind_cols(data,  ndec_list)
    
    return(res)
  }

