#' Looks to see if the \code{plcf} of a fish seems faulty
#' 
#' Flags \code{plcf} records that seem faulty in various ways. Takes a single 
#' \code{plcf} and returns a code indicating whether it may be faulty and how. 
#' 
#' @param obj A \code{plcf} object
#' @param reldatetime Date and time of the release of the fish in the same numeric units
#' used in \code{obj$bpts}
#' @param upstreamthresh A threshold distance (km) of all-at-once upstream motion that 
#' disqualifies a record as unrealistic
#' @param timethresh A threshold time (days) without being detected that is tolerated 
#' before a record is recommended to remove
#' 
#' @return \code{errcheckplcf} returns a numeric code indicating whether the record seems
#' faulty and if so what seems wrong with it. Codes are:
#' N: no problem detected
#' B: detections before the release
#' U: went upsteam, in one shot, more than \code{upstreamthresh}
#' T: went for too long without being detected
#' It is possible multiple problems occur; the error codes are concatenated.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' #Not written yet but need some
#' 
#' @export

errcheckplcf<-function(obj,reldatetime,upstreamthresh,timethresh)
{
  bpts<-obj$bpts
  bptvals<-obj$bptvals
  
  res<-"N"
  
  #look for detections before release
  if (any(bpts<reldatetime))
  {
    res<-"B"
  }
  
  #look for going upstream too much
  h<-diff(bptvals)
  upsum<-0
  for (counter in 1:length(h))
  {
    if (h[counter]>0)
    {
      upsum<-upsum+h[counter]
    } else
    {
      upsum<-0
    }
    if (upsum>upstreamthresh)
    {
      if (res=="N")
      {
        res<-"U"
      } else
      {
        res<-paste0(res,"U")
      }
      break
    }
  }
  
  #look for going for too long without being detected
  if (any(diff(bpts)>timethresh))
  {
    if (res=="N")
    {
      res<-"T"
    } else
    {
      res<-paste0(res,"T")
    }
  }
  
  return(res)
}