#' Plots multiple \code{lcf} objects on the same axes
#' 
#' Plots multiple \code{lcf} objects on the same axes
#' 
#' @param lcflist A list of \code{lcf} objects
#' @param bds Plot bounds for the x axis. Default value NULL corresponds to using the minimum range that includes the ranges of all the \code{lcf} objects.
#' @param filename File name (without extension) for saving the plot as a pdf. Default NULL uses the default plotting device instead.
#' 
#' @return \code{plotlcfs} returns nothing but generates a plot.
#' 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' lcf1<-lcf(c(1,2,3,4),c(1,0,1))
#' lcf2<-lcf(c(1.5,2.5,3.5),c(-1,-2))
#' h<-list(lcf1,lcf2)
#' plotlcfs(lcflist=h[1],xlabel="xlab test",ylabel="ylab test")
#' 
#' @export

plotlcfs<-function(lcflist,bds=NULL,xlabel,ylabel,filename=NULL)
{
  #error handling
  if (class(lcflist)!="list")
  {
    stop("Error in plotlcfs: lcflist must be a list of lcf objects")
  }
  for (counter in 1:length(lcflist))
  {
    if (class(lcflist[[counter]])[1]!="lcf")
    {
      stop("Error in plotlcfs: lcflist must be a list of lcf objects")
    }
  }
  if (!is.null(filename) && (class(filename)!="character" || length(filename)!=1))
  {
    stop("Error in plotlcfs: inappropriate filename argument")
  }
  if (class(xlabel)!="character" || length(xlabel)!=1)
  {
    stop("Error in plotlcfs: inappropriate xlabel argument")
  }
  if (class(ylabel)!="character" || length(ylabel)!=1)
  {
    stop("Error in plotlcfs: inappropriate ylabel argument")
  }
  
  #find the bounds if bds was NULL
  if (is.null(bds))
  {
    bds<-c(Inf,-Inf)
    for (counter in 1:length(lcflist))
    {
      bds[1]<-min(c(bds[1],lcflist[[counter]]$bpts))
      bds[2]<-max(c(bds[2],lcflist[[counter]]$bpts))
    }
  }
  
  #some more error handling
  if (!is.numeric(bds))
  {
    stop("Error in plotlcfs: bds must be numeric")
  }
  if (length(bds)!=2)
  {
    stop("Error in plotlcfs: bds must be length 2")
  }
  if (!all(is.finite(bds)))
  {
    stop("Error in plotlcfs: bds must have finite elements")
  }
  if (bds[2]<=bds[1])
  {
    stop("Error in plotlcfs: first element of bds must be less than second element")
  }
  
  #find the y-axis bounds
  ybds<-c(Inf,-Inf)
  for (counter in 1:length(lcflist))
  {
    ybds[1]<-min(c(ybds[1],lcflist[[counter]]$vals))
    ybds[2]<-max(c(ybds[2],lcflist[[counter]]$vals))
  }
  
  #now do the plotting
  if (!is.null(filename))
  {
    pdf(file=paste0(filename,".pdf"))
  }
  h<-lcflist[[1]]
  bp<-h$bpts
  vs<-h$vals
  plot(bp[1:2],rep(vs[1],2),xlim=bds,ylim=ybds,type='l',
       xlab=xlabel,
       ylab=ylabel)
  for (bpcount in 2:(length(bp)-1))
  {
    lines(bp[bpcount:(bpcount+1)],rep(vs[bpcount],2))
  }
  if (length(lcflist)>1)
  {
    for (lcfcount in 2:length(lcflist))
    {
      h<-lcflist[[lcfcount]]
      bp<-h$bpts
      vs<-h$vals
      lines(bp[1:2],rep(vs[1],2),xlim=bds,ylim=ybds)
      for (bpcount in 2:(length(bp)-1))
      {
        lines(bp[bpcount:(bpcount+1)],rep(vs[bpcount],2))
      }
    }
  }
  if (!is.null(filename))
  {
    dev.off()
  }
  
  return()
}
