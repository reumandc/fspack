#' An ``average velocity'' of a \code{plcf} object.
#'
#' If f(t) represents the function underlying a \code{plcf} object, then this is 
#' [f(t+lag/2)-f(t-lag/2)]/lag, which is another \code{plcf}.
#' 
#' @param obj A \code{plcf} object 
#' @param The lag to use in the average velocity. See above formulas.
#'  
#' @return \code{avelplcf} returns a \code{plcf} object. If the input \code{obj}
#' was defined from t_0 to t_f then the output will be defined from t_0+lag/2 to 
#' t_f+lag/2.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' #need some
#' 
#' @export

avelplcf<-function(obj,lag)
{
  #need an error check for lag being too big compared to the space
  #between the first and last breakpoints
  
  #first get the breakpoints
  bpts1<-obj$bpts+lag/2
  inds<-which(bpts1>max(obj$bpts)-lag/2)
  bpts1<-bpts1[-inds]
  
  bpts2<-obj$bpts-lag/2
  inds<-which(bpts2<min(obj$bpts)+lag/2)
  bpts2<-bpts2[-inds]
  
  bpts<-union(bpts1,bpts2)
  
  #now figure out the value at those breakpoints
  bptvals<-NA*numeric(length(bpts))
  for (counter in 1:length(bpts))
  {
    bptvals[counter]<-(evalatpt(obj,bpts[counter]+lag/2)
                       -evalatpt(obj,bpts[counter]-lag/2))/lag
  }
  
  #assemble into a plcf object and return
  res<-plcf(bpts,bptvals)
  return(res)
}