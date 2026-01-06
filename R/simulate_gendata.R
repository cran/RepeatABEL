#' @title Function to simulate genotype data for the RepeatABEL package.
#'
#' @description  The function simulates n individuals and p SNPs, with linkage disequilibrium (LD) given by the LD-parameter
#' @param n Number of individuals.
#' @param p Number of SNPs.
#' @param LD An LD-parameter. LD=1 gives complete LD and LD=0 no LD.
#' @param n.chrom The size of a simulated SNP.effect.

#' @return Returns a gwaa.data2 object.
#' @author Lars Ronnegard

#' @examples
#'  set.seed(1234)
#'  Gen.Data <- simulate_gendata(n=100, p=200)
#'
#' @export
#'
simulate_gendata <- function(n=100, p=1000, LD=0.9, n.chrom=1) {
  rec.prob = 1-LD
  Z=NULL
  for (k in 1:n.chrom) {

    Z1 <- matrix(0, n, p)
    Z2 <- matrix(0, n, p)
    Z1[,1] = sample(0:1, n, replace=TRUE)
    Z2[,1] = sample(0:1, n, replace=TRUE)
    for (j in 2:p) {
      snp.change <- rbinom(n, 1, rec.prob)
      Z1[,j] = abs(Z1[,j-1] - snp.change)
      snp.change <- rbinom(n, 1, rec.prob)
      Z2[,j] = abs(Z2[,j-1] - snp.change)
    }
    Z =cbind(Z, Z1 + Z2 -1)
  }
  rownames(Z) = 1:nrow(Z)
  colnames(Z) = 1:ncol(Z)
  gen.data <- Create_gwaa_data2(genotypes=Z, chromosome = as.character(rep(1:n.chrom, each=p)))
}

