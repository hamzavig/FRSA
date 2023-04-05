#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class representing the ACTUS concept of a
#' RiskFactorConnector
#' 
#' The cash flows of ACTUS CTs may be 'linked' to (stochastic)
#' external risk factors such as interest rates or reference
#' indices. The \code{RiskFactorConnector} serves as the 
#' interface. It is in fact a collection of risk factors each
#' with the corresponding 'MarketObjectCode' as it's unique
#' identifier. 
#' Instances of R/Java risk factors may be added to the
#' \code{RiskFactorConnector} which then may be 'linked' to
#' a given ACTUS CT.
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{RiskFactor, YieldCurve, ReferenceIndex}}
#'
## @examples
## @include
#' @export
#' @rdname rfc-classes
setRefClass("RiskFactorConnector",
            fields = list(
              riskfactors = "list"
            ))

##############################################################
#' \code{RiskFactorConnector}-class constructor
#'
#' Create an instance of \code{RiskFactorConnector} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM. The created instance will be 
#' an empty collection of \code{RiskFactor} objects.
#' 
#' @param ...
#'
#' @return An object of class \code{RiskFactorConnector} 
#'          containing the reference to the Java object
#' 
## @seealso 
#'
## @include
#' @export
#' @docType methods
#' @rdname rfc-methods
#' @aliases RFConn-method
setGeneric(name = "RFConn",
           def = function(...){
             standardGeneric("RFConn")
           })

## @include
#' @export
#' @rdname rfc-methods
## @aliases 
setMethod(f = "RFConn", signature = c(),
          definition = function(...){
            object = new("RiskFactorConnector")
            pars = list(...)
            if (length(pars) > 0) 
              add(object, what = pars)

            return(object)
          })

##############################################################
#' Generic \code{add} method
#'
#' Implemented for various purposes.
#' 
#' @param object The object to which to \code{add}
#' 
#' @param what The object to \code{add}
#' 
#' @seealso remove
#'
## @examples
## @include
#' @export
#' @docType methods
#' @rdname add-methods
#' @aliases add,RiskFactorConnector,list-method
#' @aliases add,RiskFactorConnector,RiskFactor-method
setGeneric(name = "add", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("add")
           })

##############################################################
#' Adding a list of \code{\link{RiskFactor}} instances to the 
#' \code{\link{RiskFactorConnector}}
#'
#' Each element of the \code{what} parameter is expected to be
#' an instance of \code{\link{RiskFactor}} class. All elements
#' are added to the \code{RiskFactorConnector} object for later 
#' 'linking' to an ACTUS CT.
#' 
#' @param object The \code{\link{RiskFactorConnector}} object
#' 
#' @param what A list containing \code{\link{RiskFactor}} objects
#' 
#' @param ...
#' 
## @seealso 
#'
#'@examples
#' yc <- YieldCurve() # create a YieldCurve object
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'     Nodes = list(ReferenceDate = "2015-01-01", 
#'     Tenors = tenors, Rates = rates)))
#' 
#' ind <- Index() # create a ReferenceIndex object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#'            "2019-01-01")
#' values <- c(100, 110, 120, 130, 140)
#' set(ind, what=list(MarketObjectCode = "CHF_SMI",
#'     Data=list(Dates=times,Values=values)))
#'     
#' fx <- FxRate() # create an FX-Rate object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
#'            "2018-01-01", "2019-01-01")
#' values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
#' set(fx, what=list(MarketObjectCode = "CHF/USD",
#'     Data=list(Dates=times,Values=values)))   
#'     
#' rf <- RFConn() # create a RiskFactorConnector object
#' add(rf,list(yc,ind,fx)) # add all risk factors to the RiskFactorConnector
#' 
## @include
#' @export
#' @docType methods
#' @rdname add-methods
#' @aliases add-method
#' @aliases add,RiskFactorConnector,RiskFactor-method
setMethod("add", signature = c("RiskFactorConnector", "list"),
          definition = function(object, what, ...){
            for (i in what) {
              PAFEMS:::add(object, i, ...)
            }
          })

##############################################################
#' Adding a \code{\link{RiskFactor}} object to the 
#' \code{\link{RiskFactorConnector}}
#'
#' Add an instance of \code{\link{RiskFactor}} class to the 
#' \code{RiskFactorConnector} object for later 'linking' to an 
#' ACTUS CT.
#' 
#' @param object The \code{\link{RiskFactorConnector}} object
#' 
#' @param what A \code{\link{RiskFactor}} object to be added
#' 
#' @param ...
#' 
## @seealso 
#'
#'@examples
#' yc <- YieldCurve() # create a YieldCurve object
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'     Nodes = list(ReferenceDate = "2015-01-01", 
#'     Tenors = tenors, Rates = rates)))
#'     
#' rf <- RFConn() # create a RiskFactorConnector object
#' add(rf,yc) # add the single RiskFactor to the RiskFactorConnector
#' 
#' @include RiskFactor.R
#' @export
#' @docType methods
#' @rdname add-methods
#' @aliases add-method
#' @aliases add,RiskFactorConnector,list-method
setMethod("add", signature = c("RiskFactorConnector", "RiskFactor"),
          definition = function(object, what, ...){
            id <- PAFEMS:::get(what, "label")
            rf_list <- list(New = what)
            if (length(id) == 0) {
              names(rf_list) <- paste("RiskFactor_",as.character(length(object$riskfactors) + 1), sep = "")
            } else {
              names(rf_list) <- id
            }
            # object$riskfactors <- rf_list
            if (any(containsID(object, id))) {
              stop("ErrorIn::RiskFactorConnector:: Name of RiskFactor already exists. Choose different name !!!")
            }
            object$riskfactors <- c(object$riskfactors, rf_list)
          })


##############################################################
#' Generic \code{remove} method
#'
#' Implemented for various purposes.
#' 
#' @param object The object from which to \code{remove}
#' 
#' @param what 'Key' of the element in \code{object} to \code{remove}
#' 
#' @seealso add
#'
## @examples
## @include
#' @export
#' @docType methods
#' @rdname rmv-methods
#' @aliases remove,RiskFactorConnector,character-method
setGeneric(name = "remove", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("remove")
           })

##############################################################
#' Removing a \code{\link{RiskFactor}} object from the 
#' \code{\link{RiskFactorConnector}}
#'
#' Each element of the \code{what} parameter is expected to
#' point to an element in the \code{RiskFactorConnector} by
#' their 'keys'. All elements that can be found in
#' \code{RiskFactorConnector} are removed.
#' 
#' @param object The \code{\link{RiskFactorConnector}} object
#' 
#' @param what A character (vector) containing key(s) to
#'    elements in \code{object} and which are to be removed
#' 
#' @param ...
#' 
## @seealso 
#'
#'@examples
#' yc <- YieldCurve() # create a YieldCurve object
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'     Nodes = list(ReferenceDate = "2015-01-01", 
#'     Tenors = tenors, Rates = rates)))
#' 
#' ind <- Index() # create a ReferenceIndex object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#'            "2019-01-01")
#' values <- c(100, 110, 120, 130, 140)
#' set(ind, what=list(MarketObjectCode = "CHF_SMI",
#'     Data=list(Dates=times,Values=values)))
#'     
#' fx <- FxRate() # create an FX-Rate object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
#'            "2018-01-01", "2019-01-01")
#' values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
#' set(fx, what=list(MarketObjectCode = "CHF/USD",
#'     Data=list(Dates=times,Values=values)))   
#'     
#' rf <- RFConn() # create a RiskFactorConnector object
#' add(rf,list(yc,ind,fx)) # add all risk factors to the RiskFactorConnector
#' containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
#' remove(rf, "YC_Prim")
#' containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
#' 
## @include
#' @export
#' @docType methods
#' @rdname rmv-methods
#' @aliases remove-method
setMethod("remove", signature = c("RiskFactorConnector", "character"),
          definition = function(object, what, ...){
            for (i in what) {
              object$riskfactors[i] <- NULL
            }
          })


##############################################################
#' Retrieve a \code{\link{RiskFactor}} object from the 
#' \code{\link{RiskFactorConnector}}
#'
#' Each element of the \code{what} parameter is expected to
#' point to an element in the \code{RiskFactorConnector} by
#' their 'keys'. All elements that can be found in
#' \code{RiskFactorConnector} are returned.
#' 
#' @param object The \code{\link{RiskFactorConnector}} object
#' 
#' @param what A character (vector) containing key(s) to
#'    elements in \code{object} which are to be returned
#' 
#' @param ...
#'
#' @return an object of class \code{\link{RiskFactor}} with 
#'        'MarketObjectCode'=\code{what} parameter
#' 
#' @seealso add, remove
#'
#'@examples
#' yc <- YieldCurve() # create a YieldCurve object
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'     Nodes = list(ReferenceDate = "2015-01-01", 
#'     Tenors = tenors, Rates = rates)))
#' 
#' ind <- Index() # create a ReferenceIndex object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#'            "2019-01-01")
#' values <- c(100, 110, 120, 130, 140)
#' set(ind, what=list(MarketObjectCode = "CHF_SMI",
#'     Data=list(Dates=times,Values=values)))
#'     
#' rf <- RFConn() # create a RiskFactorConnector object
#' add(rf,list(yc,ind)) # add all risk factors to the RiskFactorConnector
#' get(rf, "YC_Prim")
#' 
#' @include ContractModel.R
#' @export
#' @docType methods
#' @rdname get-methods
#' @aliases get-method
setMethod(f = "get", signature = c("RiskFactorConnector","character"),
          definition = function(object, what){
            if (tolower(what) == "keys") {
              out <- names(object$riskfactors)
            } else {
              if (!any(containsID(object, what))) {
                stop("ErrorIn::RiskFactorConnector:: Riskfactor with this name does not exist !!!")
              }
              out <- object$riskfactors[[what]]
            }
            return(out)
          })

##############################################################
#' Check whether a \code{RiskFactorConnector} contains a
#' certain 'key'
#'
#' For each element of the \code{what} parameter a logical is
#' returned indicating whether (\code{TRUE}) or not 
#' (\code{FALSE}) the \code{RiskFactorConnector} contains an
#' object with the same key.
#' 
#' @param object The \code{\link{RiskFactorConnector}} object
#' 
#' @param what A character (vector) containing keys to be
#'        checked
#' 
#' @param ...
#'
#' @return \code{logical} indicating whether or not a key exists
#' 
#' @seealso add, remove
#'
#'@examples
#' yc <- YieldCurve() # create a YieldCurve object
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'     Nodes = list(ReferenceDate = "2015-01-01", 
#'     Tenors = tenors, Rates = rates)))
#' 
#' ind <- Index() # create a ReferenceIndex object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#'            "2019-01-01")
#' values <- c(100, 110, 120, 130, 140)
#' set(ind, what=list(MarketObjectCode = "CHF_SMI",
#'     Data=list(Dates=times,Values=values)))
#'     
#' fx <- FxRate() # create an FX-Rate object
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
#'            "2018-01-01", "2019-01-01")
#' values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
#' set(fx, what=list(MarketObjectCode = "CHF/USD",
#'     Data=list(Dates=times,Values=values)))   
#'     
#' rf <- RFConn() # create a RiskFactorConnector object
#' add(rf,list(yc,ind,fx)) # add all risk factors to the RiskFactorConnector
#' containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
#' 
## @include
#' @export
#' @docType methods
#' @rdname cid-methods
#' @aliases containsID,RiskFactorConnector,character-method
setGeneric(name = "containsID",
           def = function(object, id, ...){
             standardGeneric("containsID")
           })

## @include
#' @export
#' @rdname cid-methods
#' @aliases containsID-method
setMethod(f = "containsID", signature = c("RiskFactorConnector","character"),
          definition = function(object, id){
          
            out <- logical(length(id))
            names(out) <- id
            for(i in id) {
              if (i %in% names(object$riskfactors)) {
                out[i] <- TRUE
              }
            }
            return(out)
          })

## @include
#' @export
setMethod(f = "names", signature = c("RiskFactorConnector"),
          definition = function(x){
            return(names(x$riskfactors))
          })

## @include
#' @export
setMethod(f = "show", signature = c("RiskFactorConnector"),
          definition = function(object){
            x <- names(object)
            cl <- character()
            for (key in x) {
              cl <- c(cl, class(get(object, key))[1])
            }
            names(cl) <- x
            print(cl)
          })

## @include
#' @export
setMethod("[[", signature = c("RiskFactorConnector", "character"),
          definition = function(x, i) {
            if ( !is.element(i, get(x, "keys")) )
              return(NULL)
            else
              get(x, what=i)
          }
)

## @include
#' @export
setMethod("[[<-", signature = c("RiskFactorConnector", "ANY"),
          definition = function(x, i, value) {
            if (containsID (x, i)) {
              remove(x, i)
            }
            add (x, value)
            x
          }
)
