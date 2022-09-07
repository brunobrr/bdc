#' Identify records located outside a given region of interest.
#'
#' This function flags records that are outside a particular spatial region of 
#' interest (roi). ROIs can be specified in a variety of raster and vector 
#' formats.  
#'
#' @family space
#' @param data data.frame. Containing geographical coordinates.
#' @param roi spatial region of interest. Can be provided as an sf, sfc,
#' SpatialPolygons, SpatialPolygonsDataFrame, rasterLayer, spatRaster,
#' or as a file path with extension #' ".shp", ".gpkg", or ".tif".
#' @param lat character string. The column name with latitude in decimal degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#' @param byExtOnly logical. If true, occurrences are only flagged if located
#' outside the roi extent. If false, occurrences are checked for whether they are
#' located within the roi polygons or raster mask.
#' @param maskValue numeric or character. Raster value corresponding to "outside"
#' the region of interest. Ignored if roi is provided as a vector layer.
#'
#' @details This test identifies records outside a particular region of interest
#' defined by a raster or vector layer
#'
#' @return A data.frame containing the column ".outside_roi". Compliant
#' (TRUE) if coordinate is outside user defined roi; otherwise "FALSE".
#'
#' @importFrom sf st_bbox st_as_sf st_as_sfc st_within
#' @importFrom stringr str_detect
#' @importFrom terra crs rast geom vect
#'
#' @export mol_roi
#'
#' @examples
#' x <- data.frame(
#'   decimalLatitude = c(1, 2, 3, 5),
#'   decimalLongitude = c(1, 2, 3, 4)
#' )
#'
#' roi <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0, xmax = 4, ymax = 4, ymin = 0), crs = 4326))
#' 
#' mol_roi(
#'   data = x,
#'   roi  = roi,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude"
#' )
#'

mol_roi <- 
  function(data,
           roi,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           byExtOnly = FALSE,
           maskValue = NA){
    
    ### Run checks
    if (!is.data.frame(data)) {
      stop(deparse(substitute(data)), " is not a data.frame", call. = FALSE)
    }
    
    check_col(data, c(lat, lon))
    
    if(!any(c("character",
              "sf",
              "sfc",
              "SpatialPolygons",
              "RasterLayer",
              "SpatRaster")) %in% class(roi)){
      stop("The region of interest is not in a valid format.")
    }
    
    ### converts coordinates columns to numeric
    data <-
      data %>%
      dplyr::rowid_to_column("id_temp") %>%
      dplyr::mutate(decimalLatitude = as.numeric(.data[[lat]]),
                    decimalLongitude = as.numeric(.data[[lon]]),
                    .outside_roi = TRUE) 
    
    ### Screen NAs
    dataCoords %>% 
      dplyr::select(dplyr::all_of(c("id_temp", lon, lat))) %>%
      dplyr::filter(!is.na(.data[[lat]]), 
                    !is.na(.data[[lon]]))
    
    if(nrow(dataCoords) == 0) stop("No valid coordinates.")
    
    ### Read file path
    if(is.character(roi)){
      
      # check file exists
      if(!file.exists(roi)) stop("The ROI filepath is invalid.")
      
      # check proper format
      if(!any(str_detect(roi,
                     c("\\.shp$", "\\.gpkg$", "\\.tif$")))){
        stop("ROI input files must have '.shp', '.gpkg' or '.tif' file extensions.")
      }
      
      # load roi
      if(str_detect(roi, "\\.tif")){
        roi <- terra::rast(roi)
      } else{
        roi <- st_read(roi)
      }
      
    }
    
    ### Convert to sf/terra
    if("SpatialPolygons" %in% .class2(roi)) roi <- st_as_sfc(roi)
    if(class(roi) == "RasterLayer") roi <- rast(roi)
    
    ### Run appropriate check
    if(any(c("sf", "sfc") %in% class(roi))){
      
      if(!any(is.na(maskValue), is.null(maskValue))){
        warning("maskValue is ignored when ROI is provided as a vector layer.")
      }
      
      unflagged <- roi_sf(dataCoords,
                          roi,
                          lat,
                          lon)
    } else(
      unflagged <- roi_rast(dataCoords,
                            roi,
                            lat,
                            lon)
    )
    
    ### Update data frame
    data$.outside_roi[data$id_temp %in% unflagged] <- FALSE
    out <- data %>% 
      select(-id_temp)
    
    if(sum(out$.outside_roi) == 0){
      message("No coordinates were located outside the region of interest.")
    } else(
      message(paste(sum(out$.outside_roi),
                    "occurrences were flagged as outside the region of interest.",
                    "One column was added to the database."))
    )
    
    return(out)
  
  }

roi_sf <- 
  function(dataCoords,
           roi,
           lat,
           lon,
           byExtentOnly){
    
    ### Check CRS
    if(st_crs(roi) != st_crs(4326)){
      message("Reprojecting ROI to WGS84.")
      roi <- roi %>% st_transform(4326)
    }
    
    ### Filter by extent
    ext <- st_bbox(roi)
    
    crpd <- dataCoords %>% 
      dplyr::filter(dplyr::between(.data[[lat]], ext["ymin"], ext["ymax"]),
                    dplyr::between(.data[[lon]], ext["xmin"], ext["xmax"])) 
    
    
    if(any(byExtOnly, nrow(crpd) == 0)){
      unflgd <- crpd$id_temp
      
    } else{
      
      ### Consolidate roi
      suppressWarnings({
        s2_status <- sf_use_s2()
        sf_use_s2(FALSE)
          
        roi <- roi %>%
          sf::st_union() %>%
          sf::st_combine()
          
        if(s2_status) sf_use_s2(TRUE)
      })
        
      
      
      unflgd <- crpd %>%
        dplyr::mutate(within = st_within(.,
                                         roi,
                                         sparse = F)[,1]) %>%
        dplyr::filter(within) %>%
        dplyr::pull(id_temp)
    }
    
    return(unflgd)
  }

roi_rast <- 
  function(dataCoords,
           roi,
           lat,
           lon,
           byExtentOnly,
           maskValue){
    
    ### Reproject to raster CRS
    dataCoords[, c("tempX", "tempY")] <- geom(project(vect(dataCoords,
                                                   geom = c(lon, lat),
                                                   crs = "EPSG:4326"),
                                              crs(roi)),
                                      wkt = FALSE,
                                      hex = FALSE,
                                      df  = TRUE)[,c("x", "y")]
    
    ### Filter by extent
    Ext <- ext(roi)
    
    crpd <- dataCoords %>% 
      dplyr::filter(dplyr::between(tempX, Ext[1], Ext[2]),
                    dplyr::between(tempY, Ext[3], Ext[4])) 
    
    
    if(any(byExtOnly, nrow(crpd) == 0)){
      unflgd <- crpd$id_temp
      
    } else{
      if(is.null(maskValue)){
        warning("No maskValue specied. Assuming maskValue is NA")
        maskValue <- NA
      }
      smpld <- crpd %>%
        dplyr::mutate(value = terra::extract(roi,
                                              crpd[,c("tempX", "tempY")],
                                              method = "simple")[,2]) %>%
        filter(!is.na(value))
      
      if(!is.na(maskValue)){
        smpld <- smpld %>%
          filter(value != maskValue) 
      }
      
      unflgd <- smpld$id_temp
    }
    
    return(unflgd)
  }
    

    

