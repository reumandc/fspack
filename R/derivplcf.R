#' Method for the \code{plcf} class that takes the derivative
#' 
#' The derivative of a \code{plcf} object is an \code{lcf} object.
#' 
#' @param object A \code{plcf} object
#' 
#' @return \code{derivplcf} returns an object of class \code{lcf}.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' x<-c(1,2,4,12,13)
#' y<-c(0,1,-1,100,5)
#' obj<-plcf(x,y)
#' res<-derivplcf(obj)
#' 
#' @export  

derivplcf<-function(object)
{
  UseMethod("derivplcf",object)
}

#' @rdname derivplcf
#' @export
derivplcf.default<-function(object)
{
  stop("Error in derivplcf: method not defined for this class")
}

#' @rdname derivplcf
#' @export
derivplcf.plcf<-function(object)
{
  bpts<-object$bpts
  bptvals<-object$bptvals
  
  #remove repetitions, relying on the fact that a plcf object was checked
  #to be sure no repeats in bpts had different values for bptvals
  inds<-which(diff(bpts)==0)+1
  if (length(inds)>0)
  {
    bpts<-bpts[-inds]
    bptvals<-bptvals[-inds]
  }
  
  vals<-diff(bptvals)/diff(bpts)
  res<-lcf(bpts,vals)
  return(res)
}