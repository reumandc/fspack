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
  v1<-bptvals[1:(length(bptvals)-1)]
  v2<-bptvals[2:length(bptvals)]
  vals<-(v2-v1)/diff(bpts)
  res<-lcf(bpts,vals)
  return(res)
}