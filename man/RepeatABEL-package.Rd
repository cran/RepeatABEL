\name{RepeatABEL-package}
\alias{RepeatABEL-package}
\alias{RepeatABEL}
\docType{package}
\title{
GWAS for repeated observations on related individuals
}
\description{
The \code{RepeatABEL} package computes score statistic based p-values for a 
linear mixed model including random polygenic effects and a random effect for 
repeated measurements. A p-value is computed for each marker and the null 
hypothesis tested is that the additive marker effect is zero. }
\details{
\tabular{ll}{
Package: \tab RepeatABEL\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-03\cr
License: \tab GPL\cr
Depends: \tab hglm, GenABEL\cr
}
The core function is \code{\link{rGLS}} that requires an GenABEL object as input
 and produces an GenABEL object.
}
\author{
Lars Ronnegard\cr\cr
Maintainer: Lars Ronnegard <lrn@du.se>
}
\references{
Husby, A., Kawakami, T., Ronnegard, L., Smeds, L., Ellegren, H. & Qvarnstrom, A. (2015) \bold{Genome-wide association mapping in a wild avian population identifies a link between genetic and phenotypic variation in a life history trait}. \emph{Proceedings of the Royal Society B: Biological Sciences}, 282(1806), 20150156.
.\cr\cr

Ronnegard, L., Shen, X. & Alam, M. (2010). \bold{hglm: A Package for Fitting Hierarchical Generalized Linear Models}. \emph{The R Journal}, \bold{2}(2), 20-28.\cr\cr

Ronnegard et al. (2015). \bold{Increasing the power of genome wide association studies in natural populations using repeated measures: evaluation and implementation.}. \emph{Manuscript}.
}

\keyword{ package }
\seealso{
\code{\link{rGLS}}
}
