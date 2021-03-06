#' Flags imprecise geographic coordinates
#'
#' This function is used to flag records with a coordinate precision below a
#' specified number of decimal places. Coordinates with one, two, or three
#' decimal places have precision of~11.1 km, ~1.1 km, and ~111 m at the equator,
#' respectively.
#'
#' @param data: data.frame. A data.frame containing geographic coordinates.
#' @param lon: character string. The column with longitude. Default =
#' "decimalLongitude".
#' @param lat: character string. The column with latitude Default =
#' "decimalLatitude".
#' @param ndec: numeric. Containing the numbers of decimals place to be tested.
#' Default = c(0,1,2).
#'
#' @return A data.frame with logical values indicating whether values are
#' rounded by the specified decimal number (ndec). In other word, potentially
#' imprecise coordinates.
#' 
#' @export
#'
#' @examples
#' "data <- data.frame(lon = c(-55.389, -13.897, 30.678, 90.675) , lat = c(-21.345, 23.567, 16.798, -10.468))"
#' "bdc_round_dec(data = data, lon = "lon", lat = "lat", ndec = c(0, 2))"
#' 
bdc_xy_precision <-
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

