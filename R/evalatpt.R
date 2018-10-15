#' Evaluates a \code{plcf} or \code{lcf} object at a point or points.
#'  
#' Evaluates a \code{plcf} or \code{lcf} object at a point or points.
#' 
#' @param obj A \code{plcf} or \code{lcf} object.
#' @param x The point(s) at which to evaluate it (a numeric vector).
#' 
#' @return \code{evalatpt} returns the values of the function
#' represented by \code{object} at \code{x}. If \code{object}
#' is an \code{lcf} object and an entry of \code{x} equals one 
#' of the breakpoints, then NA is returned for that one. If 
#' an entry of \code{x} is outside the range spanned by the 
#' breakpoints, NA is returned.
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
  #***DAN: this is currently very inefficient, you can make it better
  
  allx<-x
  res<-NA*numeric(length(allx))
  
  for (counter in 1:length(allx))
  {
    x<-allx[counter]

    if (x<min(object$bpts) || x>max(object$bpts))
    {
      res[counter]<-NA
      next
    }
    if (x %in% object$bpts)
    {
      ind<-which(x==object$bpts)
      res[counter]<-object$bptvals[ind]
      next
    }
    
    indl<-max(which(object$bpts<x))
    indh<-min(which(object$bpts>x))
    x1<-object$bpts[indl]
    x2<-object$bpts[indh]
    y1<-object$bptvals[indl]
    y2<-object$bptvals[indh]
    res[counter]<-((x-x1)/(x2-x1))*y2+((x2-x)/(x2-x1))*y1
  }
  
  return(res)
}

#' @rdname evalatpt
#' @export
evalatpt.lcf<-function(object,x)
{
  #***DAN: this is currently very inefficient, you can make it better
  
  allx<-x
  res<-NA*numeric(length(allx))
  
  for (counter in 1:length(allx))
  {
    x<-allx[counter]
    
    if (x<min(object$bpts) || x>max(object$bpts))
    {
      res[counter]<-NA
      next
    }
    if (x %in% object$bpts)
    {
      res[counter]<-NA
      next
    }
    ind<-max(which(object$bpts<x))
    res[counter]<-object$vals[ind]
  }
  
  return(res)
}

#' @rdname evalatpt
#' @export
evalatpt.default<-function(object,...)
{
  stop("Error in evalatpt: method not defined for this class")
}
