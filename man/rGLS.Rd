% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rGLS.R
\name{rGLS}
\alias{rGLS}
\title{GWAS for Studies having Repeated Measurements on Related Individuals}
\usage{
rGLS(formula.FixedEffects = y ~ 1, genabel.data, phenotype.data,
  id.name = "id", GRM = NULL, V = NULL, memory = 1e+08)
}
\arguments{
\item{formula.FixedEffects}{Formula including the response variable and cofactors as fixed effects.}

\item{genabel.data}{An GenABEL object including marker information. This object has one observtion per individuals.}

\item{phenotype.data}{A data frame including the repeated observations and IDs.}

\item{id.name}{The column name of the IDs in phen.data}

\item{GRM}{An optional genetic relationship matrix (GRM) can be included as input. Otherwise the GRM is computed within the function.}

\item{V}{An optional (co)variance matrix can be included as input. Otherwise it is computed using the hglm function.}

\item{memory}{Used to optimize computations. The maximum number of elements in a matrix that can be stored efficiently.}
}
\description{
It is used to perform genome-wide association studies on individuals that are both related and have repeated measurements.
 The function computes score statistic based p-values for a linear mixed model including random polygenic effects and
 a random effect for repeated measurements. A p-value is computed for each marker and the null hypothesis tested is a
  zero additive marker effect.
}
\details{
A generalized squares (GLS) is fitted for each marker given a (co)variance matrix V.
The computations are made fast by transforming the GLS to
an ordinary least-squares (OLS) problem using an eigen-decomposition of V.
The OLS are computed using QR-factorization. If V is not specified then a model
including random polygenic effects and permanent environmental effects is
fitted (using the hglm package) to compute V. A GenABEL object (scan.gwaa class)
is returned (including also the \code{hglm} results).
Let e.g. GWAS1 be an object returned by the \code{rGLS} function.
Then a Manhattan plot can be produced by calling \code{plot(GWAS1)} and
the top SNPs using \code{summary(GWAS1)}. Both of these functions are
generic GenABEL functions. \cr
The results from the fitted linear mixed model without any SNP effect included
are produced by calling \code{summary(GWAS1@call$hglm)}.
}
\examples{
data(Phen.Data) #Phenotype data with repeated observations
 data(gen.data) #GenABEL object including IDs and marker genotypes
 GWAS1 <- rGLS(y ~ age + sex, genabel.data = gen.data, phenotype.data = Phen.Data)
 plot(GWAS1, main="")
 summary(GWAS1)
 #Summary for variance component estimation without SNP effects
 summary(GWAS1@call$hglm)
}
\author{
Lars Ronnegard
}

