#' An S4 class to represent SNP data
#'
#' @slot results The results from the rGLS function as a data.frame
#' @slot lambda Computed inflation factor as list
#' @slot idnames Idnames as character
#' @slot map SNP order as numeric
#' @slot chromosome The chromosome name for each SNP as numeric
#' @slot call The call made by rGLS as call
#' @slot family The assumed distribution of the outcome. Only "gaussian" allowed.
#'
#' @importFrom methods setClass
#'
setClass("scan.gwaa2",
         slots=c(results="data.frame", lambda="list", idnames="character", map="numeric", chromosome="character", call="call", family="character")
)
