#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
##############################################################
#' A Reference Class extending \code{\link{RiskFactor}} class
#' and representing a yield curve risk factor
#' 
#' Yield curves define a class of market risk factors 
#' that in the ACTUS model may directly affect future cash 
#' flows arising from a financial instrument, e.g. Floating-
#' rate bonds, or are used for valuation of instruments, 
#' e.g. in discounting.
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{RiskFactor, ReferenceIndex, ForeignExchangeRate}}
#'
#' @examples
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' 
#' get(ind, "MarketObjectCode")
#' get(yc, "ReferenceDate")
#' get(yc, "Tenors")
#' get(yc, "Rates")
#' 
#' @include RiskFactor.R 
#' @export
#' @rdname ind-classes
setRefClass("YieldCurve", 
            contains = "RiskFactor",
            fields = list(ReferenceDate = "character",
                          Tenors = "character",
                          Rates = "numeric",
                          TenorDates = "character",
                          DayCountConvention = "character",
                          FUN = "function"
                          ))
                          


##############################################################
#' \code{YieldCurve}-class constructor
#'
#' Create an instance of \code{YieldCurve} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM.
#' 
#' @param ...
#'
#' @return An object of class \code{YieldCurve} 
#'          containing the reference to the Java object
#' 
#' @seealso \code{\link{ReferenceIndex, ForeignExchangeRate}}
#'
## @include
#' @export
#' @docType methods
#' @rdname yc-methods
#' @aliases YieldCurve-method
setGeneric(name = "YieldCurve",
           def = function(...){
             standardGeneric("YieldCurve")
           })

## @include
#' @export
#' @rdname yc-methods
## @aliases 
setMethod(f = "YieldCurve",signature = c(),
          definition = function(...){
            
            pars <- list(...)
            # if pars is a data.frame, convert to list()
            
            # fill fields with NULL values
            fill_fields <- list()
            fill_fields$label <- "Generic_Yield_Curve"
            fill_fields$ReferenceDate <- as.character(today())
            fill_fields$Tenors <- "0M"
            fill_fields$Rates <- Inf
            fill_fields$DayCountConvention <- "30E360"
            fill_fields$FUN <- NULL
            
            if (length(names(pars)) != 0) {
              
              # check if some fields are incorrectly specified!
              all_fields <- names(getRefClass("YieldCurve")$fields())
              pars_names <- names(pars)
              test_pars_names <- pars_names %in% all_fields
              if (!all(test_pars_names)) {
                stop(paste("ErrorInYieldCurve:: Yield Curve has no field called: ", 
                           pars_names[!test_pars_names], "!!!"))
              }
              
              # check if necessary fields are missing and set the provide field names
              if (length(names(pars)) > 1) {
                if (!all(c("ReferenceDate","Tenors","Rates") %in% names(pars))) {
                  stop(paste(
                    "ErrorInYieldCurve:: If any, all of the following fields have to be provided: "
                    , paste(c("ReferenceDate","Tenors","Rates"),collapse = ", "), "!!!"))
                } else {
                  fill_fields$ReferenceDate <- pars$ReferenceDate
                  fill_fields$Tenors <- pars$Tenors
                  fill_fields$Rates <- pars$Rates
                }
              }
              
              # use if label is provided,
              if ("label" %in% pars_names) {
                fill_fields$label <- pars$label
              }
              if ("DayCountConvention" %in% pars_names) {
                fill_fields$DayCountConvention <- pars$DayCountConvention
              }
              if ("FUN" %in% pars_names) {
                fill_fields$FUN <- pars$FUN
              }
              
            }

            yc <- new("YieldCurve")
            for (nms in names(fill_fields)) {
                yc[[nms]] <- fill_fields[[nms]]
            }

            test.dates(yc$ReferenceDate)
            yc$TenorDates <- tenors2dates(yc$ReferenceDate, yc$Tenors)
            return(yc)
            })

##############################################################
#' \code{MarketInterestRate}
#'
#' Constructor for a \code{\link{YieldCurve}} object with constant
#' rates for all tenors.
#' 
#' @param rate a numeric to set the constant market rate.
#' 
#' @param date a character indicating the reference date.
#' 
#' @param label a character to name the \code{\link{YieldCurve}} object.
#'
#' @return an object of type \code{\link{YieldCurve}}. 
#' 
#' @usage MarketInterestRate(rate, refDate, label, ...)
#' 
#' @examples
#' yc_flat <- MarketInterestRate(0.05, "2015-01-01", "MarketRate")
#' 
#' @export
setGeneric(name = "MarketInterestRate",
           def = function(rate, date, ...){
             standardGeneric("MarketInterestRate")
           })

#' @export
setMethod(f = "MarketInterestRate", signature = c("numeric","character"),
          definition = function(rate, date, label = "MarketInterestRate", ...){
            yc <- YieldCurve()
            tenors <- c("1W", "6M", "1Y", "5Y", "10Y", "50Y", "100Y")
            rates <- rep(1, length(tenors)) * rate
            set(yc, list(label = label,
                        ReferenceDate = date,
                        Tenors = tenors,
                        Rates = rates))
            set(yc, list(...))
            return(yc)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ReferenceIndex,list-method
setMethod(f = "set", signature = c("YieldCurve", "list"),
          definition = function(object, what, ...){

            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.yieldcurve.field(i)) {
                value <- what[[i]]
                switch(i,
                       ReferenceDate = {
                         object$ReferenceDate <- value
                       },
                       Rates = {
                         object$Rates <- value
                       },
                       Tenors = {
                         object$Tenors <- value
                         object$TenorDates <- tenors2dates(object$ReferenceDate, value)
                       },
                       DayCountConvention = {
                         object$DayCountConvention <- value
                       },
                       label = {
                         object$label <- value
                       }
                )
              } else {
                warning(paste("Error In Yield Curve:: Field ", i, 
                              " does not exist, cannot assign value!", sep = ""))
              }
            }
            if (length(object$Tenors) != length(object$Rates)) {
              stop("ErrorInYieldCurve::set:: Rates must have same length as Tenors")
            }
          })


## @include
#' @export
#' @rdname get-methods
#' @aliases get,RiskFactorConnector,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ReferenceIndex,character-method
setMethod(f = "get", signature = c("YieldCurve", "character"),
          definition = function(object, what, ...){
            out <- list()
            if (length(what) == 1 && tolower(what) == "all") {
              what <- names(object$getRefClass()$fields())
            }
            for (i in what) {
              if (is.valid.yieldcurve.field(i)) {
                out[[i]] <- switch(i,
                                   label = object$label,
                                   ReferenceDate = object$ReferenceDate,
                                   Tenors = object$Tenors,
                                   Rates = object$Rates,
                                   DayCountConvention = object$DayCountConvention,
                                   TenorDates = object$TenorDates
                )
              } else {
                warning(paste("ErrorInYieldCurve::get:: Field ", i, 
                              " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })

## @include
#' @export
setMethod(f = "show", signature = c("YieldCurve"),
          definition = function(object){
            cat(paste0("Label: ", object$label,"\n"))
            cat(paste0("ReferenceDate: ", object$ReferenceDate,"\n"))
            cat(paste0("DayCountConvention: ", object$DayCountConvention,"\n"))
            if (length(unique(object$Rates))==1) {
              cat(paste0("MarketInterestRate: ", round(object$Rates[1]*100, 2),"%","\n"))
              cat("Constant for all tenors/terms.")
            } else {
              curve <- object$Rates
              names(curve) <- object$Tenors
              print("Curve:")
              print(curve)
            }
            if (!is.null(body(object$FUN))) {
              cat("\nFUN:\n")
              print(object$FUN)
              # cat("fParams:\n")
              # print(object$fParams)
            }
          })

## @include
#' @export
setMethod(f = "names", signature = c("YieldCurve"),
          definition = function(x){
            return(names(x$getRefClass()$fields()))
          })


#' @export
#' @rdname yc-methods
#'
setGeneric(name = "shiftYieldCurve",
           def = function(yc, spread){
               standardGeneric("shiftYieldCurve")
           })

#' @export
#' @rdname yc-methods
#' 
setMethod(f = "shiftYieldCurve", signature = c("YieldCurve", "numeric"),
          definition = function(yc, spread){
            
            rates <- get(yc, "Rates")
            rates <- rates + spread
            set(yc, list(Rates=rates))
            
            return(yc)
          })


