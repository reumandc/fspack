#' Plots multiple \code{plcf} objects on the same axes
#' 
#' Plots multiple \code{plcf} objects on the same axes
#' 
#' @param plcflist A list of \code{plcf} objects
#' @param bds Plot bounds for the x axis. Default value NULL corresponds to using the minimum range that includes the ranges of all the \code{plcf} objects.
#' @param filename File name (without extension) for saving the plot as a pdf. Default NULL uses the default plotting device instead.
#' @param xlim Plotting limits for x-axis, default uses extent of the data
#' @param ylim Plotting limits for y-axis, default uses extent of the data
#' @param plotdetections If TRUE, plots a point for each detection
#' 
#' @return \code{plotplcfs} returns nothing but generates a plot.
#' 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' 
#' @export

plotplcfs<-function(plcflist,bds=NULL,xlabel,ylabel,filename=NULL,
                    cols=rep("black",times=length(plcflist)),xlim=NULL,ylim=NULL,plotdetections=FALSE)
{
  #error handling
  if (class(plcflist)!="list")
  {
    stop("Error in plotplcfs: plcflist must be a list of plcf objects")
  }
  for (counter in 1:length(plcflist))
  {
    if (class(plcflist[[counter]])[1]!="plcf")
    {
      stop("Error in plotplcfs: plcflist must be a list of plcf objects")
    }
  }
  if (!is.null(filename) && (class(filename)!="character" || length(filename)!=1))
  {
    stop("Error in plotplcfs: inappropriate filename argument")
  }
  if (class(xlabel)!="character" || length(xlabel)!=1)
  {
    stop("Error in plotplcfs: inappropriate xlabel argument")
  }
  if (class(ylabel)!="character" || length(ylabel)!=1)
  {
    stop("Error in plotplcfs: inappropriate ylabel argument")
  }
  
  #find the bounds if bds was NULL
  if (is.null(bds))
  {
    bds<-c(Inf,-Inf)
    for (counter in 1:length(plcflist))
    {
      bds[1]<-min(c(bds[1],plcflist[[counter]]$bpts))
      bds[2]<-max(c(bds[2],plcflist[[counter]]$bpts))
    }
  }
  
  #some more error handling
  if (!is.numeric(bds))
  {
    stop("Error in plotplcfs: bds must be numeric")
  }
  if (length(bds)!=2)
  {
    stop("Error in plotplcfs: bds must be length 2")
  }
  if (!all(is.finite(bds)))
  {
    stop("Error in plotplcfs: bds must have finite elements")
  }
  if (bds[2]<=bds[1])
  {
    stop("Error in plotplcfs: first element of bds must be less than second element")
  }
  
  #find the y-axis bounds
  ybds<-c(Inf,-Inf)
  for (counter in 1:length(plcflist))
  {
    ybds[1]<-min(c(ybds[1],plcflist[[counter]]$bptvals))
    ybds[2]<-max(c(ybds[2],plcflist[[counter]]$bptvals))
  }
  
  #now do the plotting
  if (!is.null(filename))
  {
    pdf(file=paste0(filename,".pdf"))
  }
  h<-plcflist[[1]]
  bp<-h$bpts
  vs<-h$bptvals
  if (is.null(xlim)) {xlim=bds}
  if (is.null(ylim)) {ylim=ybds}
  if (plotdetections)
  {
    plot(bp,vs,xlim=xlim,ylim=ylim,type='l',
         xlab=xlabel,
         ylab=ylabel,col=cols[1])
    points(bp,vs,type='p',pch=20,cex=.5,col=cols[1])
  } else
  {
    plot(bp,vs,xlim=xlim,ylim=ylim,type='l',
         xlab=xlabel,
         ylab=ylabel,col=cols[1])
  }
  if (length(plcflist)>1)
  {
    for (plcfcount in 2:length(plcflist))
    {
      h<-plcflist[[plcfcount]]
      bp<-h$bpts
      vs<-h$bptvals
      if (plotdetections)
      {
        lines(bp,vs,col=cols[plcfcount])
        points(bp,vs,col=cols[plcfcount],pch=20,cex=0.5)
      } else
      {
        lines(bp,vs,col=cols[plcfcount])
      }
    }
  }
  if (!is.null(filename))
  {
    dev.off()
  }
  
  return()
}