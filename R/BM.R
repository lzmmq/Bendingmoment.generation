##' When a truck travels on a bridges, we use static influence line to 
##' calculate unit load bending moment. We assume truck velocity is v(m/s), 
##' and Observation frequency is Hz,so We will get a sequence of bending moments
##' within BridgeL/v time, every 1/Hz seconds, at the mid-span of bridge. 
##' 
##' We want to return a sequence of bending moments at BridgeL/v time, every 1/Hz seconds, at the mid-span of bridge.
##' and in order to make data real, we add Noiselevel into data, and we have 9 types of default trucks,
##' each has different alxes load and alxes spacings, which was indicated in internal data.
##' @title Bending moment of 9 types of truck
##' @param Trucktype 9 types of trucks, with 1-9
##' @param BridgeL Length of bridge (m)
##' @param v velocity of truck (m/s)
##' @param Hz Observation frequency.
##' @param NoiseLevel Usually 5%, 10%, 15%, 20% noise to simulate the real data.
##' @return A sequence of bending moments generated at mid-span of bridge, within BridgeL/v time, every 1/Hz seconds.
##' \item{IL}{Static Influence Line of Bridge} 
##' @author Qian Shi
##' @import "stats"
##' @export
##' @examples
##' BendingMoment(1,100,50,50,0.15)

BendingMoment <- function(Trucktype, BridgeL, v, Hz, NoiseLevel) {
    
    tryCatch({
    Trucktype <- round(Trucktype)
    stopifnot(!missing(Trucktype),!missing(NoiseLevel), 
              !missing(BridgeL),NoiseLevel >=0, NoiseLevel <=1,
              !missing(v), !missing(Hz),
              Trucktype >= 1, Trucktype <= 9,
              BridgeL>0, v >0, Hz >= v/BridgeL )
    t <- seq(1/Hz, BridgeL/v, 1/Hz)-1/Hz
    
    TruckD <- v*t
    
    L <- as.numeric(Loads[Trucktype,])

    S <- as.numeric(Spacings[Trucktype,])
    
    total <- IL(TruckD, BridgeL,0)*L[1]
    
    for(i in 2:length(L)) {
      
      total = total + IL(TruckD, BridgeL, sum(S[1:i-1]))*L[i]
    }
    RMS <- sqrt(mean(total^2))
    set.seed(32611)
    total <- total + RMS * NoiseLevel * rnorm(length(total),0,1)
    
    total[total <0] <- 0
    retval <- data.frame(T = t, M = total)
    
    return(retval)},error = function(e){
      return("Invalid input")
    })
}







