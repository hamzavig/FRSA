#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that represents all Valuation Engine objects
#' 
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to 
#' serve as parent to various implementations of Valuation
#' Engine types as a means for designing method-inheritage. 
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{DiscountingEngine, 
#'                CAPMEngine, MultiCurrencyDiscountingEngine}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname ve-classes
setRefClass("ValuationEngine",
            fields = list(
            ))

##############################################################
#' \code{ValuationEngine}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{ValuationEngine} (e.g. \code{\link{DiscountingEngine}},
#' etc). 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{DcEngine}} for 
#' \code{\link{DiscountingEngine}}. Note that it is not possible 
#' to instanciate class \code{ValuationEngine} itself but only 
#' the implementing classes extending \code{ValuationEngine}.
#' 
#' @param object If an object of class \code{jobjRef}, then 
#'        this parameter is expected to be a reference to a Java
#'        valuation engine and a new R-object of the same class is 
#'        created where the Java reference is attached as the
#'        classes' \code{jref}-field. If a character, then
#'        \code{object} is expected to be the R-class name of
#'        the valuation engine to be instantiated.
#'
#' @return An object of a class extending \code{ValuationEngine} 
#' 
#' @seealso \code{\link{DcEng}}
#'
#' @examples 
#' # example 1: create a new 'DiscountingEngine' object
#' dc = VE("DiscountingEngine")
#' 
#' # example 2: attach the reference to a Java 'DiscountingEngine'
#' #            object to a new R-'DiscountingEngine' object. Note,
#' #            the new object will refer to the same Java engine.
#' dc = VE("DiscountingEngine")
#' set(dc, what=list(
#'                    RiskFactorObjectLink="YC_CHF",
#'                    dc.spread=0.05))
#' # same.dc = VE(dc$jref)   # This command doesn't work
#'
## @include
#' @export
#' @docType methods
#' @rdname ve-methods
#' @aliases VE,jobjRef-method
#' @aliases VE,character-method
setGeneric(name = "VE",
           def = function(object){
             standardGeneric("VE")
           })

## @include
#' @export
#' @docType methods
#' @rdname ve-methods
#' @aliases VE,jobjRef-method
setMethod(f = "VE", signature = c("character"),
          definition = function(object) {
            out <- new(object)
            return(out)
          })


## @include
#' @export
#' @rdname trms-methods
#' @aliases terms,ContractType-method
#' @aliases terms,jobjRef-method
setMethod(f = "terms", signature = c("ValuationEngine"),
          definition = function(object) {
            return(names(object$getRefClass()$fields()))
          })
