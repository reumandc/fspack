#' Creator function for the \code{lcf} class
#' 
#' The \code{lcf} class represents locally constant functions.
#' \code{lcf} inherits from the \code{list} class. 
#' 
#' @param bpts A numeric vector of the breakpoints between constant pieces. Must be in strictly ascending order.
#' @param vals A numeric vector with the values of the function between the breakpoints. Must be of length one less than \code{bpts}.
#' 
#' @return \code{lcf} returns an object of class \code{lcf}. Slots are: 
#' \item{bpts}{The input} 
#' \item{vals}{The input}
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' x<-1:5
#' y<-c(.1,5,-3,4)
#' res<-lcf(x,y)
#' 
#' @export  

lcf<-function(bpts,vals)
{
  #Error handling
  if (!is.numeric(bpts) || !is.numeric(vals))
  {
    stop("Error in lcf: inputs must be numeric")
  }
  if (!is.vector(bpts))
  {
    stop("Error in lcf: bpts must be a numeric vector")
  }
  if (length(bpts)!=length(vals)+1)
  {
    stop("Error in lcf: bpts must be one longer than vals")
  }
  if (length(bpts)<2)
  {
    stop("Error in lcf: bpts must have length at least 2")
  }
  if (any(!is.finite(bpts)) || any(!is.finite(vals)))
  {
    stop("Error in lcf: inputs must contain finite values")
  }
  if (any(diff(bpts)<=0))
  {
    stop("Error in lcf: bpts must have strictly increasing values")
  }
  
  res<-list(bpts=bpts,vals=vals)
  class(res)<-c("lcf","list")
  return(res)
}
