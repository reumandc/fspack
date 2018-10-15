#' Correlation of two \code{plcf} object
#' 
#' Computes a continuous analogue of Pearson's correlation for two \code{plcf} objects
#' 
#' @param obj1 A \code{plcf} object
#' @param obj2 Another such
#' @param bds Bounds over which to compute the correlation analogue. Default NULL 
#' means to use the intersection of the bounds over which each \code{plcf} is defined. 
#' If a non-NULL value is passed, each \code{plcf} object must be defined over the
#' given range.
#' 
#' @return \code{corplcfs} returns a single number which is the correlation.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' #need some
#' 
#' @export

corplcfs<-function(obj1,obj2,bds=NULL)
{
  if ((class(obj1)!=c("plcf","list")) || (class(obj2)!=c("plcf","list")))
  {
    stop("Error in corplcfs: obj1 and obj2 must be plcf objects")
  }
  
  #extract components of input
  bpts1<-obj1$bpts
  bpts2<-obj2$bpts
  bptvals1<-obj1$bptvals
  bptvals2<-obj2$bptvals
  
  #establish the bounds
  if (is.null(bds))
  {
    bds<-c(max(c(bpts1[1],bpts2[1])),
           min(c(bpts1[length(bpts1)],bpts2[length(bpts2)])))
    if (bds[1]>=bds[2])
    {
      stop("Error in corplcfs: obj1 and obj2 not both defined for any range of values")
    }
  }
  
  #error handling for bds
  if (!is.numeric(bds))
  {
    stop("Error in corplcfs: bds argument must be numeric or NULL")
  }
  if (length(bds)!=2)
  {
    stop("Error in corplcfs: bds argument must be a length-2 numeric vector or NULL")
  }
  if (bds[1]<max(c(bpts1[1],bpts2[1])) || bds[2]>min(c(bpts1[length(bpts1)],bpts2[length(bpts2)])))
  {
    stop("Error in corplcfs: bds must not include values for which one or both of obj1 or obj2 is not defined")
  }
  
  #compute the result
  
  #make a common set of breakpoints for the two functions and get values at those breakpoints
  bpts<-unique(sort(c(bds,bpts1[bpts1>bds[1] & bpts1<bds[2]],bpts2[bpts2>bds[1] & bpts2<bds[2]])))
  bptvals1<-evalatpt(obj1,bpts)
  bptvals2<-evalatpt(obj2,bpts)
  #we now know both functions are linear between breakpoints for this common set
  
  #compute averages for the two functions
  len<-length(bpts)
  bar1<-sum(diff(bpts)*(bptvals1[1:(len-1)]+bptvals1[2:len])/2)
  bar2<-sum(diff(bpts)*(bptvals2[1:(len-1)]+bptvals2[2:len])/2)
  
  #compute the numerator and denominator of the correlation
  num<-0
  denom1<-0
  denom2<-0
  for (counter in 1:(len-1))
  {
    x1<-bpts[counter]
    x2<-bpts[counter+1]
    y11<-bptvals1[counter]
    y12<-bptvals1[counter+1]
    y21<-bptvals2[counter]
    y22<-bptvals2[counter+1]
    
    #numerator
    m1<-(y12-y11)/(x2-x1)
    m2<-(y22-y21)/(x2-x1)
    b1<-y11-m1*x1-bar1
    b2<-y21-m2*x1-bar2
    thisnum<-(m1*m2/3)*(x2^3-x1^3)+((m1*b2+b1*m2)/2)*(x2^2-x1^2)+b1*b2*(x2-x1)
    num<-num+thisnum
    
    #first part of denom
    thisdenom1<-(m1^2)*(x2^3-x1^3)/3 + b1*m1*(x2^2-x1^2) + (x2-x1)*b1^2
    denom1<-denom1+thisdenom1
    
    #second part of denom
    thisdenom2<-(m2^2)*(x2^3-x1^3)/3 + b2*m2*(x2^2-x1^2) + (x2-x1)*b2^2
    denom2<-denom2+thisdenom2
  }
  
  return(num/sqrt(denom1*denom2))
}