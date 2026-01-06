#' @title Creates a gwaa.data2 object
#'
#' @description
#' Creates a gwaa.data2 object from input.
#'
#' @param genotypes A matrix with genotype values coded as (0,1,2) or (-1,0,1)
#' @param chromosome An array of characters for the chromosomes. Length equal to the number of SNPs.
#' @param map An array with the order of the SNPs.
#' @param phenotypes A data frame including columns with phenotypes and a column with ids, called "id"
#' @return Returns a gwaa.data2-object.
#' @author Lars Ronnegard
#'
#' @importFrom methods new
#' @export
#'
Create_gwaa_data2 <- function(genotypes, chromosome=NULL, map=NULL, phenotypes=NULL) {
  genabel.data <- methods::new("gwaa.data2")
  genabel.data@gtdata@gtps = genotypes
  genabel.data@gtdata@nids = nrow(genotypes)
  genabel.data@gtdata@nsnps = ncol(genotypes)
  genabel.data@gtdata@idnames = rownames(genotypes)
  genabel.data@gtdata@snpnames = colnames(genotypes)
  if (!is.null(chromosome)) {
    genabel.data@gtdata@chromosome = chromosome
  } else {
    genabel.data@gtdata@chromosome = rep(" ", ncol(genotypes))
  }

  if (!is.null(map)) {
    genabel.data@gtdata@map = map
  } else {
    genabel.data@gtdata@map = 1:ncol(genotypes)
  }
  if (!is.null(phenotypes)) {
    genabel.data@phdata = phenotypes
  } else {
    genabel.data@phdata = data.frame(id=1:nrow(genotypes))
  }
  return(genabel.data)
}
