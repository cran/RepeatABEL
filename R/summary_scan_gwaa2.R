#' @title Summary function for the rGLS output
#'
#' @description
#' Creates a Manhattan plot using a slimmed version of the summary.scan.gwaa() function in the GenABEL package
#'
#' @param object A scan.gwaa2 object created by the rGLS function
#' @param ... Possible additional parameters (not used in the current version)
#' @return Returns a data frame with estimated SNP effects, standard errors, test-statistic values, p-values, and corrected p-values.
#' @author Lars Ronnegard
#'
#' @export
#'
#'
"summary.scan.gwaa2" <-
  function(object, ...) {
    if (!is(object,"scan.gwaa2")) stop("object argument must be of scan.gwaa-class")
    object <- object@results
    ix <- order(object[,"P1df"])[1:10] #,index.return=TRUE)$ix[1:top]
    out <- object[ix,]
    cat("Summary for top 10 results, sorted by P1df \n")
    out
  }
