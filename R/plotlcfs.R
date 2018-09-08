#' Plots multiple \code{lcf} objects on the same axes
#' 
#' Plots multiple \code{lcf} objects on the same axes
#' 
#' @param lcflist A list of \code{lcf} objects
#' @param bds Plot bounds for the x axis. Default value NA corresponds to using the minimum range that includes the ranges of all the \code{lcf} objects.
#' @param filename File name (without extension) for saving the plot as a pdf. Default NA uses the default plotting device instead.
#' 
#' @return \code{plotlcfs} returns nothing but generates a plot.
#' 
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' 
#' @export

plotlcfs<-function(lcflist,bds,filename)
{
  #error handling
  if (class(lcflist)!="list")
  {
    stop("Error in plotlcfs: lcflist must be a list of lcf objects")
  }
  for (counter in 1:length(lcflist))
  {
    if (class(lcflist[[counter]])!="lcf")
    {
      stop("Error in plotlcfs: lcflist must be a list of lcf objects")
    }
  }
  if (class(filename)!="character")
  {
    stop("Error in plotlcfs: inappropriate filename argument")
  }
  
  #find the bounds if bds was NA
  bds<-c(Inf,-Inf)
  for (counter in 1:length(lcflist))
  {
    bds[1]<-min(c(bds[1],lcflist[[counter]]$bpts))
    bds[2]<-max(c(bds[2],lcflist[[counter]]$bpts))
  }
  
  #some more error handling, by now bds is not NA
  if (!is.numeric(bds)))
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
  if (!is.na(filename))
  {
    pdf(file=paste0(filename,".pdf"))
  }
  h<-lcflist[[1]]
  bp<-h$bpts
  vs<-h$vals
  plot(bp[1:2],rep(vs[1],2),xlim=bds,ylim=ybds)
  for (bpcount in 2:(length(bp)-1))
  {
    lines(bp[bpcount:(bpcount+1)],rep(vs[bpcount],2))
  }
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
  if (!is.na(filename))
  {
    dev.off()
  }
  
  return()
}
