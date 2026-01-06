#' @title Extracts the number of ids
#'
#' @description
#' Gets nids.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#' @return Returns the number of individuals.
#' @author Lars Ronnegard
#'
#' @export
#'
nids <- function(genabel.data) {
  return(genabel.data@gtdata@nids)
}
