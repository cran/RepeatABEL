#' @title Function to estimate lambda
#'
#' @description
#' Estimates lambda from P-values. Most code copied from the archived GenABEL package
#'
#' @param data An array of P-values
#' @param plot Logical. TRUE to produce a plot
#' @param method Either "regression" or "median".
#' @param filter Logical. If TRUE the extreme P-values are not included in the estimate of lambda.
#' @return Returns a list with estimate and standard error.
#' @author Lars Ronnegard
#'
#' @importFrom stats qchisq
#' @importFrom stats ppoints
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom graphics par
#' @importFrom graphics abline
#'
#'
#' @export
#'
estlambda <- function(data, plot=FALSE, method="regression", filter=TRUE) {
  df=1
  data <- data[which(!is.na(data))]
  ntp <- length(data)
  if ( ntp<10 ) warning(paste("number of points is too small:", ntp))
  if ( min(data)<0 ) stop("data argument has values <0")
  if ( max(data)<=1 ) {
    data <- qchisq(data, 1, lower.tail=FALSE)
  }
  if (filter)
  {
    data[which(abs(data)<1e-8)] <- NA
  }
  data <- sort(data)
  ppoi <- ppoints(data)
  ppoi <- sort(qchisq(ppoi, df=df, lower.tail=FALSE))
  data <- data[1:ntp]
  ppoi <- ppoi[1:ntp]
  out <- list()
  if (method=="regression") {
    s <- summary( lm(data~0+ppoi) )$coeff
    out$estimate <- s[1,1]
    out$se <- s[1,2]
  } else if (method=="median") {
    out$estimate <- median(data, na.rm=TRUE)/qchisq(0.5, df)
    out$se <- NA
  } else {
    stop("'method' should be either 'regression' or 'median'!")
  }

  if (plot) {
    lim <- c(0, max(data, ppoi,na.rm=TRUE))
    #		plot(ppoi,data,xlim=lim,ylim=lim,xlab="Expected",ylab="Observed", ...)
    #oldmargins <- par()$mar
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    oldmargins <- oldpar()$mar
    oldpar(mar=oldmargins + 0.2)
    plot(ppoi, data,
         xlab=expression("Expected " ~ chi^2),
         ylab=expression("Observed " ~ chi^2)
         )
    abline(a=0, b=1)
    abline(a=0, b=out$estimate, col="red")
    #par(mar=oldmargins)
  }
  on.exit
  out
}
