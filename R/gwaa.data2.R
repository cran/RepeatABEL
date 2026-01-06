#' An S4 class to represent GWAS input data
#'
#' @slot phdata Phenotype information including id
#' @slot gtdata object of class \code{\link{snp.data}} with genotype information
#'
#' @include snp.data.R
#'
#' @importFrom methods setClass
#'
setClass("gwaa.data2",
         slots=c(
           phdata="data.frame",
           gtdata="snp.data"
           )
)
