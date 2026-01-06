#' @title Extracts the id names
#'
#' @description
#' Gets the idnames.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#' @return Returns an array with the names of the individuals (as character).
#' @author Lars Ronnegard
#'
#' @export
#'
idnames <- function(genabel.data) {
  return(genabel.data@gtdata@idnames)
}
