#' @title Fits a linear mixed model (without fixed SNP effects) and computes the fitted variance-covariance matrix for later use in the rGLS function.
#'
#' @description Uses a GenABEL-like object and phenotype data as input. The model is fitted using the \code{hglm} function in the hglm package.
#'
#' @param fixed A formula including the response and fixed effects
#' @param random A formula for the random effects
#' @param id.name The column name of the IDs in phen.data
#' @param genabel.data An GenABEL-like object including marker information. This object has one observation per individual.
#' @param phenotype.data A data frame including the repeated observations and IDs.
#' @param corStruc A list specifying the correlation structure for each random effect. The options are: \code{"Ind"} for iid random effects, \code{"GRM"} for a correlation structure given by a genetic relationship matrix, or \code{"CAR"} for a spatial correlation structure given by a Conditional Autoregressive model specified by a neighborhood matrix.
#' @param GRM A genetic relationship matrix. If not specified whilst the \code{"GRM"} option is given for \code{corStruc} then the GRM is computed internally within the function.
#' @param Neighbor.Matrix A neighborhood matrix having non-zero value for an element (i,j) where the observations i and j come from neighboring locations. The diagonal elements should be zero.
#' @param verbose If TRUE the progress of the computations is printed.
#' @return Returns a list including the fitted hglm object \code{fitted.hglm}, the variance-covariance matrix \code{V} and the ratios between estimated variance components for the random effects divided by the residual variance, \code{ratio}.
#' @author Lars Ronnegard
#'
#' @importFrom stats terms
#' @importFrom stats model.matrix
#' @importFrom stats as.formula
#' @importFrom stats model.frame
#' @importFrom stats gaussian
#' @importFrom methods is
#' @importFrom hglm CAR
#'
#' @examples
#'  ####### FIRST EXAMPLE USING GRM #############
#'  set.seed(1234)
#'  Gen.Data <- simulate_gendata(n=100, p=200)
#'  Phen.Data <- simulate_PhenData(y ~ 1, genabel.data=Gen.Data,
#'                                n.obs=rep(4, nids(Gen.Data)), SNP.eff=2, SNP.nr=100, VC=c(1,1,1))
#'  GWAS1 <- rGLS(y ~ 1, genabel.data = Gen.Data, phenotype.data = Phen.Data)
#'  plot(GWAS1, main="")
#'  summary(GWAS1)
#'  #Summary for variance component estimation without SNP effects
#'  summary(GWAS1@@call$hglm)
#'  #The same results can be computed using the preFitModel as follows
#'  fixed = y ~ 1
#'  Mod1 <- preFitModel(fixed, random=~1|id, genabel.data = Gen.Data,
#'                      phenotype.data = Phen.Data, corStruc=list( id=list("GRM","Ind") ))
#'  GWAS1b <- rGLS(fixed, genabel.data = Gen.Data,
#'                 phenotype.data = Phen.Data, V = Mod1$V)
#'  plot(GWAS1b, main="Results using the preFitModel function")
#'
#' @export
#'
preFitModel <- function(fixed = y~1, random = ~1|id, id.name = "id", genabel.data, phenotype.data, corStruc = NULL, GRM = NULL, Neighbor.Matrix = NULL, verbose = TRUE) {
  if (!inherits(fixed, "formula") || length(fixed) != 3) stop("\n Fixed-effects model must be a formula of the form \"resp ~ pred\"")
  if (!inherits(random, "formula")) stop("\n Random part must be a one-sided formula of the form \" ~ effect|Cluster\"")
  if (attr(terms(random), "response") != 0) stop("\n Random part must be a one-sided formula of the form \" ~ effect|Cluster\"")
  if (all.names(random)[2] != "|") stop("The subjects/clusters in Random must be separated by \" |\"")
  if ( !is(genabel.data,"gwaa.data2") ) stop("The input of genabel.data is not a gwaa.data2 object")
  if ( is.null(genabel.data@phdata$id) ) stop("IDs not given as id in the phdata list")
  #Get trait name
  trait <- all.vars(fixed)[1]
  #Remove NAs from phenotypic data
  y.all <- phenotype.data[,names(phenotype.data) %in% trait]
  phenotype.data <- phenotype.data[!is.na(y.all),]
  #Connect IDs in GenABEL data set with IDs in the phenotype file
  id1 <- phenotype.data[,names(phenotype.data) %in% id.name] #ID for phenotype data
  id2 <- genabel.data@phdata$id #ID for genotype data
  test1 <- id1 %in% id2
  test2 <- id2 %in% id1
  ##genabel.data <- genabel.data[test2,] #Exclude individuals having no phenotype information
  genabel.data <- keep_gwaa_data(genabel.data, which(test2)) #Exclude individuals having no phenotype information
  phenotype.data = phenotype.data[test1, ] #Exclude individuals having no genotype information
  id1 <- phenotype.data[,names(phenotype.data) %in% id.name] #ID for phenotype data for cleaned data
  id2 <- genabel.data@phdata$id #ID for genotype data for cleaned data
  ### Create design matrix for the fixed effects ###
  Y <- phenotype.data[,names(phenotype.data) %in% trait]
  X <- model.matrix(fixed, data = phenotype.data)
  if (nrow(X) != length(Y)) stop("Remove all lines with NA from the phenotype input data before running this function.")
  row.names(X) <- NULL
  ###############################################
  ## Create design matrix for random effects ####
  ###############################################
  #Construct incidence matrix for repeated observations
  N <- length(id2)
  n <- length(id1)
  indx <- numeric(n)
  for (i in 1:N) {
    indx <- indx + i * (id1 %in% id2[i])
  }
  ### Get SQRT of GRM first
  if (is.null(GRM)) {
    autosomalMarkers <- which(chromosome(genabel.data) != "X")
    ##GRM <- compute.GRM(genabel.data[ , snpnames(genabel.data)[autosomalMarkers]])
    GRM <- compute.GRM(genabel.data@gtdata@gtps[ , snpnames(genabel.data)[autosomalMarkers]])
  }
  eig <- eigen(GRM)
  if (max(diag(GRM))>1.6) warning("There seems to be highly inbred individuals in your data")
  if (min(eig$values) < -0.5 ) warning("The genetic relationship matrix is far from positive definite")
  non_zero.eigenvalues = eig$values>(1e-6) #Put numerically small eigenvalues to zero
  eig$values[!non_zero.eigenvalues] <- 0
  if (verbose) cat("GRM ready \n")
  Z.GRM <- ( eig$vectors %*% diag(sqrt(eig$values)) )[indx,]
  ##Create Z
  RanTermVec <- unlist(strsplit(attr(terms(random), "term.labels"), split = c("+"), fixed = TRUE))
  k.rand <- length(RanTermVec)
  Z <- NULL
  RandC <- NULL
  data <- phenotype.data
  for (i in 1:k.rand) {
    RanTerm <- unlist(strsplit(RanTermVec[i], split = "|", fixed = TRUE))
    RanTerm[2] <- gsub(" ", "", RanTerm[2], fixed = TRUE)
    ranf <- paste("~", "as.factor(", RanTerm[2], ")", "-1", sep = "")
    ranf <- as.formula(ranf)
    rmf <- model.frame(ranf, data)
    z <- model.matrix(attr(rmf, "terms"), data = rmf)
    if (nrow(z) != length(Y)) stop("Remove all lines with NA from the phenotype input data before running this function.")
    row.names(z) <- NULL
    if (is.null(corStruc)) {
      RandC <- c(RandC, ncol(z))
      Z <- cbind(Z, z)
    }else{
      cs <- unlist( corStruc[names(corStruc) %in% RanTerm[2]] )
      for (j in 1:length(cs)) {
        zL <- switch(cs[j],
                     "Ind" = z,
                     "GRM" = Z.GRM,
                     "CAR" = z,
                     stop("Input correlation structure not available")
        )

        RandC <- c(RandC, ncol(zL))
        Z <- cbind(Z, zL)
        j.rand <- length(RandC)
        if (cs[j]=="CAR") {
          if ( is.null(Neighbor.Matrix) ) stop("CAR correlation requires a Neighbor.Matrix to be specified")
          if ( ncol(z) != ncol(Neighbor.Matrix) ) stop("Incorrect dimension of Neighbor.Matrix")
          if ( j.rand == 1) {
            rand.family = list( CAR(D=Neighbor.Matrix) )
          } else {
            rand.family[[j.rand]] = CAR(D=Neighbor.Matrix)
          }
        } else {
          if (j.rand == 1) rand.family = list(gaussian())
          if (j.rand > 1) rand.family[[j.rand]] = gaussian()
        }
      }
    }
  }
  #####################################
  ### Fit the model
  if (length(RandC) > 1) mod1 <- hglm(y=Y, X=X, Z=Z, RandC=RandC, maxit=200, rand.family=rand.family)
  if (length(RandC) == 1) mod1 <- hglm(y=Y, X=X, Z=Z, RandC=RandC, maxit=200, rand.family=rand.family[[1]])
  ratio <- mod1$varRanef/mod1$varFix
  #stop("The constructV function needs to be changed if CAR model is to be used")
  #V <- constructV(Z, RandC, ratio)
   V = diag(nrow(Z))
    indx = 1
    for (i in 1:length(ratio)) {
    	Z.tmp <- Z[, indx:(indx + RandC[i] - 1)]
		indx = indx + RandC[i]
    	if (rand.family[[i]]$family != "CAR") {
			V <- V + tcrossprod(Z.tmp) * ratio[i]
    	} else {
    		ratio[i] <- mod1$CAR.tau/mod1$varFix
    		V <- V + Z.tmp %*% solve( diag( ncol(Z.tmp) ) - mod1$CAR.rho * Neighbor.Matrix ) %*% t(Z.tmp) * ratio[i]
    	}
    }
  list( fitted.hglm = mod1, V = V, ratio = ratio)
}
