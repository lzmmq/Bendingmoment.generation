##' Static Influence Line of Bridge 
##'
##' The bending moment of the truck passing this bridge can be calculated by the static influence line.
##' The influence line equation for the bending moment of unit load can be expressed as IL function.
##' @title Influence line equation
##' @param TruckD Truck's position at the bridge.(TruckD <= BridgeL)
##' @param BridgeL length of Bridge
##' @param SpaceL Spacings between axle loads
##' @return Bending moment influence line by unit load
##' @author Qian Shi

IL <- function(TruckD, BridgeL,SpaceL){
  
  tryCatch({
    
    stopifnot(!missing(TruckD), !missing(BridgeL),!missing(SpaceL),
              BridgeL > 0 )
    
    retval <- rep(0, length(TruckD))
    TruckD <- TruckD - SpaceL
    for(i in 1:length(TruckD)) {
      
      if(TruckD[i] < 0)
      {retval[i] <- 0}
      
      else if(TruckD[i] < BridgeL/2)
      {retval[i] <- TruckD[i]/2}
      
      else
      {retval[i] <- BridgeL/2-TruckD[i]/2}
    }
    return(retval)}, error = function(e){
      return("invalid input")
    })
}



