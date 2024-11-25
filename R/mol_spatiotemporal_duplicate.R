#' Identify spatiotemporally duplicated records
#'
#' This function flags records that are spatialtemporal duplicates
#'
#' @param data data.frame. Containing information about the location, taxonomy,
#' date, and source of the observation.
#' @param lat character string. The column name with latitude in decimal degree
#' and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degree and
#' in WGS84. Default = "decimalLongitude".
#' @param date character string. The column name with temporal information of the occurrence record. 
#' @param recordCols character string. Names of columns that distinguish records collected simultaneously (e.g., specimenID).
#' @param priorityCol character. Name of column that indicates how to prioritize which duplicate record is flagged.
#' @param priorityOrder a vector of values in the priority column in descending order of priority.
#' @param ndec integer. Number of decimal places in the decimalDegree coordinates at which to determine spatial duplicates.
#'
#' @details Records with NA in the lat, lon, or date columns are excluded. If 'date' argument is NULL, function flags spatial duplicates and the flag column name is changed accordingly. 
#' 
#' priorityCol is used to arrange the data prior to identifying duplicates. The vector in priorityOrder is used to define levels in a factor prior to ordering. If priorityOrder is NULL and priorityCol isn't, priorityCol is simply sorted in ascending order. Values in the priority column that are not specified in the priorityOrder will be arranged as lowest priority, with a warning.
#'
#' @return A data.frame containing the column ".spatiotemporal_duplicate", or ".spatial_duplicate" when date = NULL.
#' .Compliant (TRUE) if if observation is a spatiotemporal duplicate; otherwise "FALSE".
#'
#' @importFrom lubridate as_date
#'
#' @export
#'
#' @examples

bdc_spatiotemporal_duplicate <- function(
    data,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    date = "eventDate",
    recordCols = NULL,
    priorityCol = NULL,
    priorityOrder = NULL,
    digits = 3){
  
  ### Run checks
  if (!is.data.frame(data)) {
    stop(deparse(substitute(data)), " is not a data.frame", call. = FALSE)
  }
  
  dup_cols <- c(lon, lat, date, recordCols)
  
  check_col(data, c(dup_cols, priorityCols))
  
  if(!is.null(priorityOrder) & is.null(priorityCols)){
    stop("No column specified for sorting by priority.\nEither specify a valid column name as priorityCol or set priorityOrder to NULL.")
  }
  
  # check mismatches between priorityCol and priorityOrder
  if(!is.null(priorityCol){
    if(is.null(priorityOrder)){
      warning("The order of priority for values in the priority column has not been specified.\nValues will be prioritized in ascending order.")
    }
    
    if(!is.null(priorityOrder)){
      if(!any(priorityOrder %in% unique(data[[priorityCol]]))){
          stop("The values specified as the priority order do not appear in the priority column. Recheck arguments.")
      }
      
      if(!all(priorityOrder %in% unique(data[[priorityCol]]))){
        warning("Not all values in the priority order occur in the priority column.\nThese values will be ignored.")
      }
      
      if(!all(unique(data[[priorityCol]]) %in% priorityOrder)){
        warning("Not all values in the priority column are included in the priority order.\nThese values will be treated as lowest priority.")
      }
    }
  } 
  
  if(!is.numeric(ndec)) stop("the number of decimals (ndec) argument must be provided as an integer.")
  
  if(as.integer(ndec) != ndec) stop("the number of decimals ('ndec') argument must be provided as an integer.")
  
  ### update data
  flagCol <- dplyr::if_else(is.null(date),
                            ".spatial_duplicate",
                            ".spatiotemporal_duplicate")
  data <- data %>%
    dplyr::mutate(decimalLatitude = as.numeric(.data[[lat]]),
                  decimalLongitude = as.numeric(.data[[lon]]),
                  !! flagCol := FALSE) %>%
    tibble::rowid_to_column("id_temp") 


  ### subset and rearrange data
  data_temp <- data %>%
    dplyr::select(dplyr::all_of(c(lat, lon, date, recordCols, priorityCol))) %>%
    dplyr::filter(!dplyr::if_any(c(lon, lat, date), ~is.na(.))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(lon, lat)), ~round(., digits = ndec)))
  
  if(!is.null(priorityCol)){
    if(!is.null(priorityOrder)){
      data_temp[[priorityCol]] <- factor(data_temp[[priorityCol]],
                                           levels = priorityOrder)
    }
    
    data_temp <- data_temp %>% 
      dplyr::arrange(across(all_of(priorityCol)))
  }
  
  ### get flagged ids
  flagged <- data_temp %>%
    filter(duplicated(dplyr::select(., all_of(dup_cols)))) %>%
    pull(id_temp)
  
  ### update flag col
  data[[flagCol]][data$id_temp %in% flagged] <- TRUE
  
  message(
    paste(
      "\nbdc_spatiotemporal_duplicate:\nFlagged",
      sum(data[[flagCol]]),
      "observations that were",
      paste0(str_remove(flagCol, "^\\."), "s."),
      "\nOne column was added to the database.\n"
    )
  )
  
  return(data)
  
}

