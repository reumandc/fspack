#' Evaluates a \code{plcf} or \code{lcf} object at a point.
#'  
#' Evaluates a \code{plcf} or \code{lcf} object at a point.
#' 
#' @param obj A \code{plcf} or \code{lcf} object.
#' @param x The point at which to evaluate it.
#' 
#' @return \code{evalatpt} returns the values of the function
#' represented by \code{object} at \code{x}. If \code{object}
#' is an \code{lcf} object and \code{x} equals one of the 
#' breakpoints, then NA is returned. If \code{x} is outside 
#' the range spanned by the breakpoints, NA is returned.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' #Not written yet but need some
#' 
#' @export

evalatpt<-function(object,...)
{
  UseMethod("evalatpt",object)
}

#' @rdname evalatpt
#' @export
evalatpt.plcf<-function(object,x)
{
  if (x<min(object$bpts) || x>max(object$bpts))
  {
    return(NA)
  }
  if (x %in% object$bpts)
  {
    ind<-which(x==object$bpts)
    return(object$bptvals[ind])
  }
  indl<-max(which(object$bpts<x))
  indh<-min(which(object$bpts>x))
  x1<-object$bpts[indl]
  x2<-object$bpts[indh]
  y1<-object$bptvals[indl]
  y2<-object$bptvals[indh]
  res<-((x-x1)/(x2-x1))*y2+((x2-x)/(x2-x1))*y1
  return(res)
}

#' @rdname evalatpt
#' @export
evalatpt.lcf<-function(object,x)
{
  if (x<min(object$bpts) || x>max(object$bpts))
  {
    return(NA)
  }
  if (x %in% object$bpts)
  {
    return(NA)
  }
  ind<-max(which(object$bpts<x))
  return(object$vals[ind])
}

#' @rdname evalatpt
#' @export
evalatpt.default<-function(object,...)
{
  stop("Error in evalatpt: method not defined for this class")
}
