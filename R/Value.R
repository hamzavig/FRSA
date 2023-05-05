#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the value at AD0 of a contract
#'
#' Different value-concepts can be derived from a financial instrument
#' or the resulting EventSeries, respectively. Currentently, these are
#' Nominal value and Mark-to-Model value.
#'
#' Nominal value is the value of a Maturity instrument that is currently 
#' outstanding as a dept from the counterparty on the liability side to
#' the party on the asset side. The payment pattern of the nominal value
#' over time is what distincts different Maturity instrument types from
#' each other.
#' Actus CT algorithms generate the nominal value over time together with
#' cash flows. Hence, the nominal value for a certain analysis date may 
#' only be derived from the first-level results of a Maturity contract.
#'
#' Mark-to-model value is the value of a financial instrument according
#' to a certain valuation model. E.g. for Maturity instruments, this is usually
#' its net-present-value. Mostly, this value is used if no value can be observed 
#' for this instrument on the market. This can be the case if the market for this
#' instrument is illiquid. 
#' In order for a mark-to-model value to be derived, the CT algorithm must have
#' been processed and an appropriate \code{ValuationEngine} either assigned in 
#' advance or past as method argument 'method'.
#'
#' @param object object The \code{ContractType} or \code{EventSeries}-object for which to derive the value
#'
#' @param by time as per which to compute the value
#'
#' @param type A character representing the type of value (either 'nominal' or 'market')
#' 
#' @param ... Currently unused
#' 
#' @include EventSeries.R 
#' @export
#' @docType methods
#' @rdname val-methods

setGeneric(name = "value", def = function(object, by, type, ...){
  standardGeneric("value")
})


#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "character", "character"),
          definition = function(object, by, type, digits = 2, ...){
            if(type=="nominal") {
              val = sapply(by, function(ad) {
                evs = data.frame(
                  object$events_df$time,
                  object$events_df$nominalValue,
                  object$events_df$type
                )
                colnames(evs) = c("times","values","types")
                evs$times = timeDate(evs$times)
                evs.sub = subset(evs,times<=timeDate(substring(ad,1,10)))
                if (dim(evs.sub)[1] == 0) {
                  evs.sub = data.frame(times=ad, values=0,types="AD0")
                }
                evs.last = evs.sub[which(evs.sub$times==max(evs.sub$times)),]
                return(evs.last$values[nrow(evs.last)])
              })
            } else if(type %in% c("market")) {
              
              if(length(object$riskFactors) != 0){
                yc <- object$riskFactors[[1]]
              }else{
                yc <- NULL
              }
              
              val = sapply(by, function(ad) { # loop over elements in "by"
                evs = data.frame(
                  object$events_df$time,
                  object$events_df$payoff,
                  object$events_df$type
                )
                colnames(evs) = c("times", "values", "types")
                evs$times = timeDate(evs$times)
                evs.sub = subset(evs, times >= timeDate(ad))
                evs.sub <- subset(evs.sub, !(evs.sub$types %in% c("DPR")))
                if( nrow(evs.sub)==0 | sum(evs.sub$types %in% c("IED","PRD")) > 0) {
                  # execution of contract has not yet started
                  return(0.0)
                } else {
                  cfs = evs.sub$values
                  if(!is.null(yc)){
                    dts = as.character(evs.sub$times)
                    dfs = discountFactors(yc, from=ad, to=dts)
                    return(as.numeric(cfs%*%dfs))
                  }else{
                    return(as.numeric(cfs%*%rep(1, length(cfs))))
                  }
                }
              })
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(round(val, digits))
})
