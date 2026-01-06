#' @title Computes a Genetic Relationship Matrix from a GenABEL-like object
#'
#' @description
#' One method for GRM computations implemented.
#'
#' @param gen.data The GenABEL-like object.
#' @return Returns a genomic relationship matrix.
#' @author Lars Ronnegard
#'
#' @export
#'
compute.GRM <-
  function(gen.data) {
      SNP.matrix <- scale(as.matrix(gen.data))
      m <- ncol(SNP.matrix)
      markers.to.fit <- (1:m)[!is.na(colMeans(SNP.matrix))]
      GRM <- tcrossprod(SNP.matrix[,markers.to.fit])/length(markers.to.fit)
      rm(SNP.matrix)
    return(GRM)
  }
