#' @title Function to plot P-values as a Manhattan plot
#'
#' @description
#' Creates a Manhattan plot
#'
#' @param x A scan.gwaa2 object created by the rGLS function
#' @param y A parameter not used in the current version
#' @param ystart Lowest value on the y-axis
#' @param col Default is c("blue","green")
#' @param sort Logical. If TRUE the SNPs are sorted before plotting.
#' @param ylim Limits of the y-axis
#' @param main Plot title
#' @param ... Possible additional parameters (not used in the current version)
#'
#' @return No return value, called for side effects
#' @author Lars Ronnegard
#'
#' @importFrom graphics points
#' @importFrom graphics axis
#' @importFrom methods is
#' @export
#'
"plot.scan.gwaa2" <-
  function (x, y, ..., ystart=0, col=c("blue","green"), sort=TRUE, ylim, main = NULL) {
    df=1
    df <- as.character(df)
    dfpars <- c("1","2","Pc1df","Pc2df")
    if (!(any(dfpars==df))) stop ("df parameter must be 1, 2, \"Pc1df\", or \"Pc2df\"")
    if (!is(x,"scan.gwaa2")) stop("Plot must be done on an object of scan.gwaa2-class")
    ##if (length(map(x)) != length(x[,"P1df"])) stop("length of map and scan points not equal!")
    xx=x
    x=x@results

    if (length(xx@map) != length(x[,"P1df"])) stop("length of map and scan points not equal!")
    #if (any(names(x) == "Pgw1df")) if (!is.null(x$Pgw1df)) {x$P1df <- x$Pgw1df; x$P2df <- x$Pgw2df}


    Pv <- x[,"P1df"]
    if (df=="2") {Pv <- x[,"P2df"];}
    else if (df=="Pc1df") {Pv <- x[,"Pc1df"];}
    else if (df=="Pc2df") {Pv <- x[,"Pc2df"];}

    if (sort) {
      ##newmap <- sortmap.internal(chromosome(x),map(x),delta=delta)
      neworder <- order(xx@chromosome, xx@map)
      mymap <- xx@map[neworder]
      ##mymap <- newmap$cummap
      ##mychromosome <- chromosome(x)[newmap$ix]
      mychromosome <- xx@chromosome[neworder]
      ##Pv <- Pv[newmap$ix]
      Pv <- Pv[neworder]
      ##newchnum <- newmap$chnum
      ##rm(newmap)
      gc()
    } else {
      mymap <- xx@map
      mychromosome <- xx@chromosome
    }

    Pv <- replace(Pv,(Pv<=0),1.e-16)
    maxy <- max(-log10(Pv),na.rm=TRUE)
    cargnams <- names(match.call())

    if (!missing(ylim)) {
      ylim <- ylim
    } else {
      ylim <- c(ystart,maxy)
    }

    if (dim(table(mychromosome))>1) {
      ##chind <- chrom.char2num(mychromosome) %% length(col)
      chind <- as.numeric(mychromosome) %% length(col)
      idxCH <- which(chind==0)
      plot(mymap[idxCH],-log10(Pv[idxCH]),main=main,xlab="Chromosome",
           ylab=expression(-log[10](P-value)), axes=FALSE, ylim=ylim,
           xlim=c(min(mymap),max(mymap)), col=col[length(col)])
      for (colidx in c(1:(length(col)-1))) {
        idxCH <- which(chind==colidx)
        points(mymap[idxCH],-log10(Pv[idxCH]),col=col[colidx])
      }
      mxlog <- floor(max(-log10(Pv),na.rm=T))
      if (mxlog==0) mxlog <- max(-log10(Pv),na.rm=T)
      if (mxlog<1)
        axis(2,at=ylim)
      else
        axis(2,at=c(ylim[1]:ylim[2]))
      chrs <- unique(mychromosome)
      chpos <- rep(NA,length(chrs))
      for (i in 1:length(chrs)) chpos[i] <- mean(mymap[mychromosome==chrs[i]])
      axis(1,at=chpos,labels=chrs)
    } else {
      plot(mymap,-log10(Pv),main=main,xlab="Map position",
           ylab=expression(-log[10](P-value)),
           ylim=ylim, col=col[1])
    }
  }

