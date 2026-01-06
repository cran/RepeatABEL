#' @title Extracts the chromosome numbers
#'
#' @description
#' Gets the chromosome numbers.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#'
#' @author Lars Ronnegard
#' @return Returns an array of chromosome numbers
#' @export
#'
chromosome <- function(genabel.data) {
  return(genabel.data@gtdata@chromosome)
}
