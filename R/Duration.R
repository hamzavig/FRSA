##############################################################
#' \code{duration}
#'
#' Function which calculates the duration of a portfolio or a contract.
#' 
#' @param x the contract or portfolio of contracts for which to calculate 
#'          the duration.
#' 
#' @param type a \code{character} defining the type of duration; possible types are 
#'             'fisher-weil', 'macaulay' (default) and 'dollar'. 
#'
#' @param yield a \code{numeric}, an Object of class \code{YieldCurve} or 
#'                    \code{DynamicYieldCurve} that describes the spot rate term structure
#'                    or indicates the yield used to calculate the duration.
#'  
#' @param price a \code{numeric}, indicating the price used for calculating the yield to maturity
#'              of each contract.
#' 
#' @param isPercentage a \code{numeric}, indicating if the 'yield' is passed as percentage 
#'                     (TRUE) or as fraction (FALSE). 
#' 
#' @param by a \code{character} indicating the date as for which the net present value
#'             is calculated.
#' 
#' @return a \code{numeic} containing the calculated duration.  
#' 
#' @usage duration(x, type="macaulay", yield, price, isPercentage=TRUE, by)
#' 
#' @details 
#' For the Macaulay duration, if \code{yield} is not provided, \code{price} should
#' be provided and is used to calculate the  \code{yield}.
#' For the Fisher-Weil duration, \code{yieldCurve} must be specified. In this
#' case the argument \code{price} has no effect. 
#' 
#' @include DynamicYieldCurve.R YieldCurve.R
#' @export 
#' 
setGeneric(name = "duration", def = function(object, yield, ...){
  standardGeneric("duration")
})


#' @include DynamicYieldCurve.R YieldCurve.R
#' @export
#' 
setMethod(f = "duration", signature = c("EventSeries", "YieldCurve"),
          definition = function(object, yield, by=NULL, type="fisher-weil", price=NULL, 
                                isPercentage=TRUE, digits=2, ...){
 
            events_df <- object$events_df
            
            yearFractions <- c(0)
            for(i in 2:nrow(events_df)){
              fraction <- yearFraction(as.character(events_df[1,"time"]), as.character(events_df[i,"time"]), convention = "30E360")
              yearFractions <- c(yearFractions, fraction)
            }
            
            events_df$fraction <- yearFractions
            
            evs <- events_df[,c("time","payoff","type", "fraction")]
            
            if(is.null(by)) {
              by <- as.character(evs$time[1])
            }
            
            if (evs[evs$time==by,"type"]=="IED") {
              evs <- evs[evs$time>by,]
            } else {
              evs <- evs[evs$time>=by,]
            }
            
            evs <- evs[!(evs$type %in% c("IPCI","DPR","PRF","RR","RRY","SC","PRY")),]
            evs <- evs[!((evs$type %in% "AD0") & (evs$payoff==0)),]
            
            
            if (dim(evs)[1]==0) {
              cf <- timeSeries(cbind(0,0), 
                               charvec = by, 
                               units = c("payoff","fraction"))
              d <- 0
            }else{
              if (evs$type[dim(evs)[1]]!="MD" & evs$payoff[dim(evs)[1]]==0){
                evs <- evs[1:dim(evs)[1]-1,]
              }
              
              evs.ts <- timeSeries(evs[,c("payoff","fraction")], charvec=substring(evs$time,1,10))
              evs.ts$fraction <- as.numeric(evs.ts$fraction)
              evs.ts$payoff <- as.numeric(evs.ts$payoff)
              cf <- aggregate(evs.ts, time(evs.ts), "sum")
              cf$fraction <- evs.ts[row.names(cf),]$fraction
              
              if ( as.character(time(cf)[1,]) == by ){
                cf <- cf[-1,]
              }
              
              # extract times (in years) from cash flows
              t <- cf$fraction
              # always fisher-weil duration
              df <- discountFactors(yield, to=as.character(time(cf)))
              d <- sum(t*df*cf$payoff)/t(cf$payoff)%*%df
            }
            
            return(round(as.numeric(d), digits))
})
