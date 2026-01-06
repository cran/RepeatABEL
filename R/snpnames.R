#' @title Extracts the snpnames
#'
#' @description
#' Gets the SNP names.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#'
#' @author Lars Ronnegard
#'
snpnames <- function(genabel.data) {
  return(genabel.data@gtdata@snpnames)
}
