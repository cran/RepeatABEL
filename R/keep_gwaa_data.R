#' @title A function to subset an gwaa.data2 object
#'
#' @description
#' Extracts a subset of the data.
#'
#' @param genabel.data A GenABEL-like data of class gwaa.data2.
#' @param indx.keep Indeces to extract.
#'
#' @author Lars Ronnegard
#'
keep_gwaa_data <- function(genabel.data, indx.keep = NULL) {
  #if (is.integer(indx.keep)) indx.keep = which(indx.keep)
  indx.keep <- as.numeric(indx.keep)
  genabel.data@gtdata@gtps = genabel.data@gtdata@gtps[indx.keep,]
  genabel.data@gtdata@nids = nrow(genabel.data@gtdata@gtps)
  genabel.data@gtdata@idnames = genabel.data@gtdata@idnames[indx.keep]
  tmp = as.data.frame(genabel.data@phdata[indx.keep,])
  names(tmp)=names(genabel.data@phdata)
  genabel.data@phdata = tmp
  return(genabel.data)
}
