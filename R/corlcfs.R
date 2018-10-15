#' Correlation of two \code{lcf} object
#' 
#' Computes a continuous analogue of Pearson's correlation for two \code{lcf} objects
#' 
#' @param obj1 An \code{lcf} object
#' @param obj2 Another such
#' @param bds Bounds over which to compute the correlation analogue. Default NULL 
#' means to use the intersection of the bounds over which each \code{lcf} is defined. 
#' If a non-NULL value is passed, each \code{lcf} object must be defined over the
#' given range.
#' 
#' @return \code{corlcfs} returns a single number which is the correlation.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' #need some
#' 
#' @export

corlcfs<-function(obj1,obj2,bds=NULL)
{
  if ((class(obj1)!=c("lcf","list")) || (class(obj2)!=c("lcf","list")))
  {
    stop("Error in corlcfs: obj1 and obj2 must be lcf objects")
  }
  
  #extract components of input
  bpts1<-obj1$bpts
  bpts2<-obj2$bpts
  vals1<-obj1$vals
  vals2<-obj2$vals
  
  #establish the bounds
  if (is.null(bds))
  {
    bds<-c(max(c(bpts1[1],bpts2[1])),
           min(c(bpts1[length(bpts1)],bpts2[length(bpts2)])))
    if (bds[1]>=bds[2])
    {
      stop("Error in corlcfs: obj1 and obj2 not both defined for any range of values")
    }
  }
  
  #error handling for bds
  if (!is.numeric(bds))
  {
    stop("Error in corlcfs: bds argument must be numeric or NULL")
  }
  if (length(bds)!=2)
  {
    stop("Error in corlcfs: bds argument must be a length-2 numeric vector or NULL")
  }
  if (bds[1]<max(c(bpts1[1],bpts2[1])) || bds[2]>min(c(bpts1[length(bpts1)],bpts2[length(bpts2)])))
  {
    stop("Error in corlcfs: bds must not include values for which one or both of obj1 or obj2 is not defined")
  }
  
  #compute the result
  
  #make a common set of breakpoints for the two functions
  bpts<-unique(sort(c(bds,bpts1[bpts1>bds[1] & bpts1<bds[2]],bpts2[bpts2>bds[1] & bpts2<bds[2]])))
  nvals1<-NA*numeric(length(bpts)-1)
  nvals2<-NA*numeric(length(bpts)-1)
  for (counter in 1:(length(bpts)-1))
  {
    ind<-max(which(bpts1<=bpts[counter]))
    nvals1[counter]<-vals1[ind]
    ind<-max(which(bpts2<=bpts[counter]))
    nvals2[counter]<-vals2[ind]
  }
  
  #compute averages for the two functions
  lcfbar1<-sum(nvals1*diff(bpts))/(bpts[length(bpts)]-bpts[1])
  lcfbar2<-sum(nvals2*diff(bpts))/(bpts[length(bpts)]-bpts[1])

  #compute the numerator of the correlation
  num<-0
  for (counter in 1:(length(bpts)-1))
  {
    num<-num+(nvals1[counter]-lcfbar1)*(nvals2[counter]-lcfbar2)*(bpts[counter+1]-bpts[counter])
  }
  
  #compute the denom of the correlation
  dpart1<-0
  dpart2<-0
  for (counter in 1:(length(bpts)-1))
  {
    dpart1<-dpart1+(nvals1[counter]-lcfbar1)^2*(bpts[counter+1]-bpts[counter])
    dpart2<-dpart2+(nvals2[counter]-lcfbar2)^2*(bpts[counter+1]-bpts[counter])
  }
  denom<-sqrt(dpart1*dpart2)
  
  return(num/denom)
}