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
#' @param eventDate Numeric or date. The column with event date information.
#' @param src character string. The column name denoting the source of each 
#' observation.
#' @param srcPriority character. Order in which to prioritize retaining
#' data from each source.
#' @param recordIDcols character. Columns with recordIDs distinguishing
#' samples collected at the same time and place.
#' @param cols_to_remove character. Which columns should be removed prior to
#' checking for duplicates? Default = "all", which means that all columns 
#' containing the results of data quality tests are removed.
#'
#' @details 
#'
#' @return A data.frame containing the column ".spatiotemporal_duplicate"
#' .Compliant (TRUE) if if observation is a spatiotemporal duplicate; otherwise "FALSE".
#'
#' @importFrom 
#'
#' @export
#'
#' @examples
