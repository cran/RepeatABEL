#' @title Extracts the map information
#'
#' @description
#' Gets the map.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#'
#' @author Lars Ronnegard
#'
map <- function(genabel.data) {
  return(genabel.data@gtdata@map)
}
