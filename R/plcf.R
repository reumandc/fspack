#' Creator function for the \code{plcf} class
#' 
#' The \code{plcf} class represents piecewise linear continuous functions.
#' \code{plcf} inherits from the \code{list} class. 
#' 
#' @param bpts A numeric vector of the breakpoints between linear pieces
#' @param bptvals A numeric vector with the values of the function at the breakpoints
#' 
#' @return \code{plcf} returns an object of class \code{plcf}. Slots are: 
#' \item{bpts}{The input, sorted into ascending order} 
#' \item{bptvals}{The input, rearranged so it still corresponds to the elements of \code{bpts}}
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' x<-1:5
#' y<-c(.1,5,-3,4,10)
#' res<-plcf(x,y)
#' 
#' @export  

plcf<-function(bpts,bptvals)
{
  #Error handling
  if (!is.numeric(bpts) || !is.numeric(bptvals))
  {
    stop("Error in plcf: inputs must be numeric vectors of the same length")
  }
  if (!is.vector(bpts) || !is.vector(bptvals))
  {
    stop("Error in plcf: inputs must be numeric vectors of the same length")
  }
  if (length(bpts)!=length(bptvals))
  {
    stop("Error in plcf: inputs must have the same length")
  }
  if (length(bpts)<2)
  {
    stop("Error in plcf: inputs must be numeric vectors of length at least 2")
  }
  if (any(!is.finite(bpts)) || any(!is.finite(bptvals)))
  {
    stop("Error in plcf: inputs must contain finite values")
  }
  
  inds<-order(bpts)
  bpts<-bpts[inds]
  bptvals<-bptvals[inds]
  
  if (length(unique(bpts))<length(bpts))
  {
    stop("Error in plcf: bpts must have all unique values")
  }
  
  res<-list(bpts=bpts,bptvals=bptvals)
  class(res)<-c("plcf","list")
  return(res)
}