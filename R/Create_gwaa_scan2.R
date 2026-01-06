#' @title Creates a scan.gwaa2 object
#'
#' @description
#' Creates a scan.gwaa2 object from the rGLS output.
#'
#' @param data A gwaa.data2 object
#' @param P1df P-values computed from external analysis
#' @param SNP.eff Estimated additive SNP effects
#' @return Returns a scan.gwaa2 object.
#' @author Lars Ronnegard
#'
#' @importFrom stats qchisq
#' @importFrom stats qnorm
#' @importFrom methods is
#' @export
#'
Create_gwaa_scan2 <- function(data, P1df, SNP.eff) {
	chi2.1df <- qchisq(P1df, df=1, lower.tail=FALSE)
	lambda <- try( estlambda(chi2.1df), silent=TRUE )
	se_effB <- abs(SNP.eff)/qnorm(P1df/2, lower.tail=FALSE)
    if ( is(lambda,"try-error" ) ) lambda <- list( estimate = NA, se = NA )
	results <- data.frame(effB = SNP.eff,  se_effB = se_effB, chi2.1df = NA, P1df = P1df, Pc1df = NA, stringsAsFactors = FALSE)
  rownames(results) = snpnames(data)
	out <- new("scan.gwaa2", results = results, lambda = lambda, idnames = idnames(data), map=map(data), chromosome=chromosome(data), call = as.call( list("rGLS") ), family = "gaussian")
	return(out)
}

