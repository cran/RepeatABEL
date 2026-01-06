#' An S4 class to represent SNP data
#'
#' @slot nids The number of ids as numeric
#' @slot idnames The idnames as character
#' @slot nsps The number of SNPs as numeric
#' @slot snpnames The SNP names as character
#' @slot map The order of the SNPS as numeric
#' @slot chromosome The chromosome names for each SNP as numeric
#' @slot gtps The matrix with SNP coding
#'
#' @name snp.data
#'
#' @importFrom methods setClass
#'
setClass("snp.data", slots=c(nids="numeric", idnames="character",
                                   nsnps="numeric", snpnames="character", map="numeric",
                                   chromosome="character",  gtps="matrix"))
