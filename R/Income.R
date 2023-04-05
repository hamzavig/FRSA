#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the income-vector for \code{ContractType}
#' 
#' Income of a contract is computed over a (number of) specific time
#' interval(s) defined by argument \code{by}. Within such a time interval,
#' income is composed of two components: (1) nominal income as the
#' net payments from interest and fees, and (2) income from re-valuation
#' of the contract at beginning and end of the time interval. If at the 
#' begining of the time interval the contract's 'mark-to-model" value is
#' higher than at the end a re-valuation gain results and vice versa.
#' Thereby, the valuation model may be defined for each contract prior
#' to calling this function or specified using the function parameter 
#' 'method'.
#' 
#' Different income-concepts can be derived for a financial instrument
#' or the resulting EventSeries, respectively. Currentently, these are
#' Marginal income and Cumulative income.
#'
#' Marginal income-vector represents the aggregate income from interest
#' and fee payments within a
#' set of user-defined time-intervals. The time-intervals are defined as
#' a sequence of timeDate-dates. Thereby, the marginal income-vector
#' gives the net interest+fee cash flows within the specified 
#' time-intervals.
#'
#' Cumulative income-vector is the cumulative sum over time (-intervals)
#' of the marginal income-vector.
#'
#' @param object The \code{ContractType} or \code{EventSeries}-object for which to derive the income-vector
#'
#' @param by A sequence of 'timeDate's providing the target time-axis for the income-vector
#'
#' @param type A character representing the type of income (either 'marginal' or 'cumulative')
#' 
#' @param method A 'ValuationEngine' (or list thereof) giving the valuation methods used when computing the re-valuation gains as part of income
#'    
#' @param ... (optional) Use parameter 'revaluation.gains=FALSE' in order to return income solely from 
#' interest/fee payments
#'   
#' @return A \code{numeric} object representing the income-vector on the target time-axis
#' 
#' @seealso \code{\link{ContractType}} and \code{\link{EventSeries}}
#'
#' @examples
#' pam <- Pam()
#' set(pam, what=list(
#'                  ContractID = "001",
#'                  Currency = "CHF",
#'                  Calendar = "Weekday",
#'                  ContractRole = "RPA",               
#'                  StatusDate       = "2016-05-30T00",
#'                  ContractDealDate = "2016-05-30T00",
#'                  InitialExchangeDate = "2016-05-30T00",
#'                  MaturityDate = "2020-06-01T00",
#'                  NotionalPrincipal = 1000,
#'                  NominalInterestRate = 0.05,
#'                  CycleOfInterestPayment = "1Y-", 
#'                  PremiumDiscountAtIED = 0.0,
#'                  DayCountConvention = "30E/360",
#'                  BusinessDayConvention = "SCF"))
#' ad <- "2016-06-01T00"
#' 
#' # generate event series
#' evs=events(pam, ad)
#' 
#' # define target income time axis
#' by=timeSequence(substring(ad, 1, 10), "2020-06-01", by="1 year")
#' 
#' # derive marginal income from interest and fee payments for defined time axis
#' income(pam, by, "marginal", revaluation.gains=FALSE)
#' 
#' # derive cumulative income
#' income(pam, by, "cumulative", revaluation.gains=FALSE)
#' 
#' # now include revaluation gains
#' # therefore, define market environment and valuation method
#' yc=YieldCurve()
#' set(yc, what=list(Nodes=list(ReferenceDate=ad,
#'                              Tenors=c("1M", "10Y"),
#'                              Rates=c(0.005, 0.02)),
#'                   MarketObjectCode = "RiskFreeCurve"))
#' rf=RFConn()
#' add(rf, yc)
#' dcEngine <- DcEngine()
#' set(dcEngine, list(RiskFactorObjectLink="RiskFreeCurve",
#'                   dc.spread=0.0))
#' set(dcEngine, rf)
#' 
#' # now compute income with revaluation gains for defined time axis
#' income(pam, by, "marginal", dcEngine, revaluation.gains=TRUE)
#' income(pam, by, "cumulative", dcEngine, revaluation.gains=TRUE)
#' 
## @include 
#' @export
#' @docType methods
#' @rdname inc-methods
setGeneric(name = "income", 
           def = function(object, by, type, revaluation.gains, method, ...){
  standardGeneric("income")
})


##------------------------------------------------------------------------------

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "missing", "missing"),
          definition = function(object, by, type, ...) {
            return( income(object, as.timeDate(by), type, ...))
          })


#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "missing", "missing"),
          definition = function(object, by, type, ...){
            
            pars=list(...)
            if(type=="marginal") {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })


#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...) {
            return( income(object, as.timeDate(by), type, revaluation.gains, ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            
            pars=list(...)
            if(type=="marginal") {
              if( revaluation.gains ) {
                stop("If 'revaluation.gains=TRUE', 'method' must be specified.")
              } else {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
              }
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", revaluation.gains, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })


#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...) {
            return( income(object, as.timeDate(by), type, revaluation.gains, method, ...))
          })


#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            pars=list(...)
            if(type=="marginal") {
              if( revaluation.gains ) {
                inc = income.from.payments(object, by, ...) + 
                  income.from.revaluation.from.es(object, by, method, ...)
              } else {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
              }
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", revaluation.gains, 
                                  method, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })



# income from interest/fee
income.from.payments = function(eventSeries, by, digits=2, ...) {
  # Check that ICPI is income relevant!!
  income.events <- subset(eventSeries$events_df, type %in% c("IP","IPCI","FP","OPS","DPR","IED"))
  # adjust for PremiumDiscountsatIED
  idx.ied <- income.events$type=="IED"
  income.events$payoff[idx.ied] <- income.events$payoff[idx.ied] + income.events$nominalValue [idx.ied]
  inc <- timeSeries(rep(0, length(by)), charvec = by)
  cf.raw <- timeSeries(income.events$payoff, charvec = substring(income.events$time, 1, 10))
  cf.aggr <- aggregate(cf.raw, by, FUN=sum)
  if (length(cf.aggr) > 0) {
    inc[time(cf.aggr),] <- cf.aggr
  }
  inc <- as.numeric(series(inc))[-1]
  return(round(inc, digits)) 
}


income.from.accruals.new = function(eventSeries, by, digits=2, ...) {
  
  if (nrow(eventSeries$events_df[eventSeries$events_df[,"type"]=="AD0",]) > 0){
    if (is.nan(eventSeries$events_df[eventSeries$events_df[,"type"]=="AD0","nominalAccrued"])) {
      eventSeries$events_df[eventSeries$events_df[,"type"]=="AD0","nominalAccrued"] <- 0
    }
  }
  
  ev.df <- eventSeries$events_df[, c("time","type","nominalAccrued")]
  ev.df$time <- substring(ev.df$time, 1, 10)
  ev.target <- data.frame(time = c(as.character(min(by) - 24 * 60 * 60), as.character(by)), type = c("Init",rep("Accr",length(by))),
                          nominalAccrued = c(0, rep(NA,length(by))))
  colnames(ev.target) <- c("time", "type", "nominalAccrued")
  ev.df <- rbind(ev.df, ev.target)
  ev.ts <- zoo(ev.df, 1:length(ev.df$time))
  ev.ts$nominalAccrued <- na.locf(ev.ts$nominalAccrued)
  deltaAccr <- c(diff(as.numeric(coredata(subset(ev.ts, type == "Accr")$nominalAccrued))))
  return(round(deltaAccr, digits))
}



# income from revaluation using eventSeries
income.from.revaluation.from.es = function(eventSeries, by, method, digits=2, ...) {
  # compute aggregate principal cash flows which are added/deducted
  # from delta-market-values between by-times
  times <- eventSeries$events_df$time
  types <- eventSeries$events_df$type
  payoffs <- eventSeries$events_df$payoff
  pr.cf <- aggregate(
    timeSeries(
      c(rep(0, length(by)), payoffs[types %in% c("IED","PR","MD")]),
      c(by, timeDate(substring(times[types %in% c("IED","PR","MD")], 1, 10)))
    ), by = by, FUN = sum)
  
  # compute mark-to-model values
  if (is.null(method)) {
    vals <- PAFEMS::value(eventSeries, as.character(by), type = "market", digits = digits)
  } else {
    vals <- PAFEMS::value(eventSeries, as.character(by), type = "market", method = method, digits = digits)
  }

  inc <- as.numeric( diff(vals) + pr.cf[-1])
  return(round(inc, digits))
}