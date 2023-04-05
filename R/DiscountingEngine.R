#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A \code{\link{ValuationEngine}} implementing discounting of
#' future cash flows
#' 
#' This class represents a \code{\link{ValuationEngine}} that
#' uses the future cash flow discounting method to compute a
#' value of a CT. The engine is implemented in Java so that
#' this class points to an instance of the respective Java
#' class. 
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{DiscountingEngineModel, ValuationEngine}}
#'
## @examples
#' 
#' @include ValuationEngine.R 
#' @include RiskFactor.R
#' @include YieldCurve.R
#' @export 
#' @rdname dcv-classes
setRefClass("DiscountingEngine",
            contains = c("ValuationEngine"),
            fields = list(dc.spread = "numeric",
                          dc.object = "RiskFactor"
            ))

## @include
#' @export
#' @rdname vem-methods
setGeneric(name = "DcModel",
           def = function(...){
             standardGeneric("DcModel")
           })

## @include 
#' @export
#' @rdname ve-methods
setGeneric(name = "DcEngine",
           def = function(rf, ...){
             standardGeneric("DcEngine")
           })

## @include
#' @export
#' @rdname ve-methods
setMethod(f = "DcEngine", signature = c("missing"),
          definition = function(...){
              object <- new("DiscountingEngine")
              pars <- list(...)
              if (!("dc.spread" %in% names(pars))) {
                pars <- c(pars, list(dc.spread=0))
              }
              if (length(pars) == 0) {
              } else if (is.list(pars[[1]])) {
                  set(object = object, what = pars[[1]])
              } else {
                  set(object = object, what = pars)
              }
              return(object)
          })

#' @include RiskFactorConnector.R
#' @export
#' @rdname ve-methods
setMethod(f = "DcEngine", signature = c("RiskFactorConnector"),
          definition = function(rf, ...){
            object <- DcEngine(...)
            set(object, rf)
            return(object)
          })

setMethod(f = "set",
          signature = c("DiscountingEngine","list"),
          definition = function(object, what) {
            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.dcengine.field(i)) {
                value <- what[[i]]
                switch(i,
                       dc.object = {
                         object$dc.object <- value
                       },
                       dc.spread = {
                         object$dc.spread <- value
                       }
                )
              } else {
                warning(paste("ErrorInDiscountingEngine:: Field ", i, " does not exist, cannot assign value!", sep = ""))
              }
            }
          })

setMethod(f = "set",
          signature = c("DiscountingEngine","RiskFactorConnector"),
          definition = function(object, what) {
            # get the yield curve out and give an error if more than one yield curve is defined
            is_yc <- unlist(lapply(what$riskfactors, 
                                   function(x) ((class(x) == "YieldCurve") || (class(x) == "DynamicYieldCurve"))))
            if (sum(as.numeric(is_yc)) > 1) {
              stop("ErrorIn::DiscountingEngine:: Only one YieldCurve can be defined in RiskFactorConnector for DCEngine !!!")
            }
            object$dc.object <- what$riskfactors[is_yc==TRUE][[1]]
          })


## @include
#' @export
#' @rdname get-methods
#' @aliases get,ContractType,character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngine,character-method
#' @aliases get,jobjRef,character-method
setMethod(f = "get", signature = c("DiscountingEngine","character"),
          definition = function(object, what){
            if (what == "dc.object") {
                out <- object$dc.object
              } else if (what == "dc.spread") {
                out <- object$dc.spread
              } 
            return(out)
          })

## @include
#' @export
setMethod("show", signature = "DiscountingEngine",
          definition = function(object){
            cat(paste0("DiscountingSpread: ", object$dc.spread,"\n"))
            print("RiskFactorObject:")
            print(object$dc.object)
          })

## -----------------------------------------------------------------
## helper methods
# existing fields in the DiscountingEngine class
validDCEngineFields <- function() {
  return(c("dc.object", "dc.spread"))
}

# check if fields are valid
is.valid.dcengine.field <- function(x) {
  valid <- validDCEngineFields()
  return(x %in% valid)
}


