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
#' yc <- DynamicYieldCurve()
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
#' @include RiskFactor.R YieldCurve.R
#' @export
#' @rdname ind-classes
setRefClass("DynamicYieldCurve", 
            contains = "RiskFactor",
            fields = list(ReferenceDate = "character",
                          Tenors = "character",
                          Rates = "data.frame",
                          TenorDates = "data.frame",
                          DayCountConvention = "character",
                          FUN = "function"
                          # ,fParams = "list"
                          )
            )



##############################################################
#' \code{DynamicYieldCurve}-class constructor
#'
#' Create an instance of \code{DynamicYieldCurve} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM.
#' 
#' @param ...
#'
#' @return An object of class \code{DynamicYieldCurve} 
#'          containing the reference to the Java object
#' 
#' @seealso \code{\link{ReferenceIndex, ForeignExchangeRate}}
#'
## @include
#' @export
#' @docType methods
#' @rdname yc-methods
#' @aliases DynamicYieldCurve-method
setGeneric(name = "DynamicYieldCurve",
           def = function(...){
             standardGeneric("DynamicYieldCurve")
           })

## @include
#' @export
#' @rdname yc-methods
## @aliases 
setMethod(f = "DynamicYieldCurve",signature = c(),
          definition = function(...){
            
            pars <- list(...)
            # browser()
            
            # if pars is a data.frame, convert to list()
            
            # fill fields with NULL values
            fill_fields <- list()
            fill_fields$label <- "Generic_Yield_Curve"
            fill_fields$ReferenceDate <- as.character(today())
            fill_fields$Tenors <- "0M"
            fill_fields$Rates <- setNames(data.frame(NA), fill_fields$Tenors)
            rownames(fill_fields$Rates) <- fill_fields$ReferenceDate
            fill_fields$DayCountConvention <- "30E360"
            fill_fields$FUN <- NULL
            # fill_fields$fParams <- NULL
            
            if (length(names(pars)) != 0) {
              
              # check if some fields are incorrectly specified!
              all_fields <- names(getRefClass("DynamicYieldCurve")$fields())
              pars_names <- names(pars)
              test_pars_names <- pars_names %in% all_fields
              if (!all(test_pars_names)) {
                stop(paste("ErrorInDynamicYieldCurve:: Yield Curve has no field called: ", 
                           pars_names[!test_pars_names], "!!!"))
              }
              
              if (is.data.frame(pars$Rates)) {
                fill_fields$ReferenceDate <- rownames(pars$Rates)
                fill_fields$Tenors <- colnames(pars$Rates)
                fill_fields$Rates <- pars$Rates
              } else if (length(names(pars)) > 1) {
                # check if necessary fields are missing and set the provide field names
                if (!all(c("ReferenceDate","Tenors","Rates") %in% names(pars))) {
                  stop(paste(
                    "ErrorInDynamicYieldCurve:: If any, all of the following fields have to be provided: "
                    , paste(c("ReferenceDate","Tenors","Rates"),collapse = ", "), "!!!"))
                } else {
                  fill_fields$ReferenceDate <- pars$ReferenceDate
                  fill_fields$Tenors <- pars$Tenors
                  fill_fields$Rates <- setNames(data.frame(t(pars$Rates)),pars$Tenors) 
                  rownames(fill_fields$Rates) <- fill_fields$ReferenceDate
                }
                # if ( "label" %in% names(pars) ){
                #   print("Have label")
                #   fill_fields$label <- pars$label
                # }
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
                # if ( !("fParams" %in% pars_names) ) {
                #   stop(paste("If 'FUN' is defined, 'fParams' must be defined, too!"))
                # } 
                # fill_fields$fParams <- pars$fParams
              }
            }
            
            yc <- new("DynamicYieldCurve")
            for (nms in names(fill_fields)) {
              yc[[nms]] <- fill_fields[[nms]]
            }
            
            test.dates(yc$ReferenceDate)
            yc$TenorDates <- tenors2dates(yc$ReferenceDate, yc$Tenors, frame=TRUE)
            return(yc)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ReferenceIndex,list-method
# setMethod(f = "set", signature = c("DynamicYieldCurve", "list"),
#           definition = function(object, what, ...){
setMethod(f = "set", signature = c("DynamicYieldCurve", "missing"),
          definition = function(object, ...){
              what <- list(...)
              set(object=object, what=what)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ReferenceIndex,list-method
setMethod(f = "set", signature = c("DynamicYieldCurve", "list"),
          definition = function(object, what, ...){

            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.yieldcurve.field(i)) {
                value <- what[[i]]
                switch(i,
                       ReferenceDate = {
                         if (length(object$ReferenceDate) != length(value)) {
                           stop(paste("ErrorIn::DynamicYieldCurve::set::", 
                                      "Field is not allowed to change dimensions !!!"))
                         }
                         object$ReferenceDate <- value
                         object$TenorDates <- tenors2dates(object$ReferenceDate,
                                                                object$Tenors,frame=TRUE)
                       },
                       Rates = {
                         object$Rates <- value
                         object$Tenors <- colnames(value)
                         object$ReferenceDate <- rownames(value)
                         object$TenorDates <- tenors2dates(rownames(value),
                                                                colnames(value),frame=TRUE)
                       },
                       Tenors = {
                         stop("Not yet implemented !!!")
                         object$Tenors <- value
                         object$TenorDates <- tenors2dates(yc$ReferenceDate, 
                                                                value, frame=TRUE)
                         # here it would be necessary to also compute the rates again 
                         # if I allow to change this!
                       },
                       DayCountConvention = {
                         object$DayCountConvention <- value
                       },
                       label = {
                         object$label <- value
                       },
                       FUN = {
                         object$FUN <- value
                         # if ( !("fParams" %in% par.names) ) {
                         #   stop(paste("If 'FUN' is defined, 'fParams' must be defined, too!"))
                         # } 
                         # object$fParams <- what$fParams
                       }
                )
              } else {
                warning(paste("ErrorInDynamicYieldCurve:: Field ", i, 
                              " does not exist, cannot assign value!", sep = ""))
              }
            }
            if (length(object$Tenors) != length(object$Rates)) {
              stop("ErrorInDynamicYieldCurve::set:: Rates must have same length as Tenors")
            }
          })


## @include
#' @export
#' @rdname get-methods
#' @aliases get,RiskFactorConnector,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ReferenceIndex,character-method
setMethod(f = "get", signature = c("DynamicYieldCurve", "character"),
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
                warning(paste("ErrorInDynamicYieldCurve::get:: Field ", i, 
                              " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })


#' @export
setMethod(f = "add", signature = c("DynamicYieldCurve", "data.frame"),
          definition = function(object, what, ...){
            if (!identical(object$Tenors, colnames(object$Rates))) {
              stop(paste("ErrorIn::DynamicYieldCurve::add::",  
                         "Tenors of rates added must match the existing YieldCurve's Tenors"))
            }
            # add new row
            object$Rates <- rbind(object$Rates, what)
            # order by row names
            object$Rates <- object$Rates[ order(rownames(object$Rates)), ]
            object$ReferenceDate <- rownames(object$Rates)
            object$TenorDates <- tenors2dates(object$ReferenceDate, 
                                                   colnames(object$Rates), frame=TRUE)
          })

#' @export
setMethod(f = "add", signature = c("DynamicYieldCurve", "DynamicYieldCurve"),
          definition = function(object, what, ...){

            if (!identical(object$Tenors, what$Tenors)) {
              stop(paste("ErrorIn::DynamicYieldCurve::add::", 
                         "Tenors of rates added must match the existing YieldCurve's Tenors"))
            }

            if (!identical(object$DayCountConvention, what$DayCountConvention)) {
              stop(paste("ErrorIn::DynamicYieldCurve::add::", 
                         "DayCountConvention of rates added must match the existing YieldCurve"))
            }
            
            yc <- DynamicYieldCurve()
            set(yc, what=list(Rates=rbind(object$Rates, what$Rates),
                              label = object$label,
                              DayCountConvention = object$DayCountConvention))
            return(yc)
          })

#' @export
setMethod(f = "add", signature = c("YieldCurve", "YieldCurve"),
          definition = function(object, what, ...){
            return(add(as.DynamicYieldCurve(object),as.DynamicYieldCurve(what)))
          })

#' @export
setMethod(f = "add", signature = c("YieldCurve", "data.frame"),
          definition = function(object, what, ...){
            return(add(as.DynamicYieldCurve(object),what))
          })

#' @export
setMethod(f = "add", signature = c("DynamicYieldCurve", "YieldCurve"),
          definition = function(object, what, ...){
            return(add(object,as.DynamicYieldCurve(what)))
          })

################################################################################
#' Computes the forward rate from time t1 to time t2.
#' 
#' This function still needs documentation, yes.
#' 
#' @export
setGeneric(name = "getRateAt",
           def = function(object, from, to, ...){
             standardGeneric("getRateAt")
           })

#' @export
setMethod(f = "getRateAt",
          signature = c("YieldCurve", "character", "character"),
          definition = function(object, from, to, method = "continuous", 
                                period = "Y", refdate = NULL, ...){
            # convert YieldCurve to dynamic first...
            return(getRateAt(as.DynamicYieldCurve(object), from, to, method = method, 
                               period = period, refdate = refdate, ...))
          })

################################################################################
#' @export
setMethod(
  f = "getRateAt",
  signature = c("DynamicYieldCurve", "character", "character"),
  definition = function(object, from, to, method = "continuous", period = "Y", 
                        refdate = NULL, ...){
            
    # check that 'from' and 'to' have same length if both have length > 1
    if (length(from)>1 && length(to)>1) {
      if (length(from)!=length(to)){
        stop(paste("ErrorIn::DynamicYieldCurve::getRateAt:: 'from' and 'to'", 
             "dates do not have valid length !!! "))
      }
    }
        
    # set refdate to earliest date if its not provided
    if (is.null(refdate)){
      refdate <- min(c(from, to))
    }
    # print(refdate)
    # print(object$ReferenceDate[1])
    # check if any of the dates are before first reference date of yield curve
    if ((refdate < object$ReferenceDate[1]) | 
        (min(c(from, to)) < object$ReferenceDate[1])) {
      stop(paste("ErrorIn::DynamicYieldCurve::getRateAt::", 
           "No Yields can be calculated before first ReferenceDate of the DynamicYieldCurve!!!"))
    }
            
    # expand necessary dates to have same length for both "to" and "from"
    if (length(from) < length(to)){
      from <- rep(from, length(to))
    } else if (length(to) < length(from)) {
      to <- rep(to, length(from))
    }
            
    # due to discountfactor, from > to is also allowed, 
    # but the result should be the same, so switch exchange from and to
    for (i in 1:length(from)) { 
      if (from[i] > to[i]) {
        helper_to <- to[i]
        to[i] <- from[i]
        from[i] <- helper_to
      }
    }
            
    # get relevant reference date which is earlier than from & to
    ref_idx <- max(cumsum(object$ReferenceDate <= refdate))
    # Tests if 'refdate' is before or at the same time of both, 'from' and 'to' 
    if (refdate <= min(c(from, to))) {# the minimum should always in 'from'
      # get necessary year fractions
      t1 <- yearFraction(object$ReferenceDate[ref_idx], 
                         from, object$DayCountConvention)
      t2 <- yearFraction(object$ReferenceDate[ref_idx], 
                         to, object$DayCountConvention)

      # ----------Hier Fallunterscheidung Funktion oder numerische Werte---------
      # define the interpolator
      if (is.null(body(object$FUN))) {
        interpolator <- Interpolator(
          xValues = yearFraction(
            object$ReferenceDate[ref_idx], as.character(object$TenorDates[ref_idx,]), 
            object$DayCountConvention), yValues = as.numeric(object$Rates[ref_idx,]))
        
        # get rates from interpolation
        s1 <- interpolator$getValueAt(t1)
        s2 <- interpolator$getValueAt(t2)
      } else {
        # browser()
        vars <- c(list(t=t1), list(...))
        s1 <- do.call(object$FUN, vars)
        vars <- c(list(t=t2), list(...))
        s2 <- do.call(object$FUN, vars)
      }
      # ----------Ende Fallunterscheidung-----------------------------------------
              
      # calculate forward rate
      if (method == "linear") {
        out <- ((1+s2*t2)/(1+s1*t1)-1)/(t2-t1)
      } else if (method == "compound") { 
        # QUESTION:
        # What happens if (t2-t1)*num_period is not an integer?
        num_period <- convert.rate.period(period)
        out <- num_period * ( 
          (
            ( (1+s2/num_period)^(t2*num_period)/
                (1+s1/num_period)^(t1*num_period) 
              )^( 1/( (t2-t1)*num_period ) ) 
            ) - 1
          )
      } else if (method == "continuous") {
        out <- (t2*s2 - t1*s1)/(t2 - t1)
      } else {
        stop(paste("ErrorIn::DynamicYieldCurve::getRateAt2:: Method ", 
                   method, " not supported !!!"))
      }
              
    } else {
      # HERE GOES IMPLEMENTATION OF DYNAMIC PART
      # Question:
      # What exactly happens here?
      warning(paste("RATES SELECTED WITH REFDATE AFTER INITIAL DATE.\\", 
                    "NOTE: FUNCTIONALITY NOT PROPERLY TESTED YET !!!"))
      out <- rep(NA, length(from))
      for (k in 1:length(from)) {
                
        # clipping of 'from' vector 
        ref_idx_from <- max(cumsum(object$ReferenceDate <= from[k])) 
          
        # pre-allocate memory for necessary rates and time deltas
        rates <- rep(NA, ref_idx)
        dt <- rep(NA, ref_idx)
                
        warning(paste(
          "Here the method 'getRateAt_old' was called.\\",
          "It has been changed to a call of 'getRateAt'. Please check if correct."))
        rates[1] <- getRateAt( 
          object,from[k],object$ReferenceDate[ref_idx_from+1], 
          method = method, period = period, 
          refdate = object$ReferenceDate[ref_idx_from])
        dt[1] <- yearFraction(from[k], object$ReferenceDate[ref_idx_from+1], 
                              object$DayCountConvention)
                
        # prepare the while loop
        count <- 2
        idx <- ref_idx_from + 1
        while (idx < ref_idx) {
          dt[count] <- yearFraction(
            object$ReferenceDate[idx], object$ReferenceDate[idx+1], 
            object$DayCountConvention)
          interpolator_spot <- Interpolator(
            xValues = yearFraction(
              object$ReferenceDate[idx], as.character(object$TenorDates[idx,]), 
              object$DayCountConvention), yValues = as.numeric(object$Rates[idx,]))
                  
          rates[count] <- interpolator_spot$getValueAt(dt[count])
          idx <- idx + 1
          count <- count + 1
        }
        dt[count] <- yearFraction(object$ReferenceDate[idx], to[k], 
                                  object$DayCountConvention)
        interpolator_spot <- Interpolator(
          xValues = yearFraction(
            object$ReferenceDate[idx], as.character(object$TenorDates[idx,]), 
            object$DayCountConvention), yValues = as.numeric(object$Rates[idx,]))
        rates[count] <- interpolator_spot$getValueAt(dt[count])
        T <- yearFraction(from[k], to[k], object$DayCountConvention)
        if (method == "linear") {
          out[k] <- (prod( rates * dt + 1 ) - 1)/T
        } else if (method == "compound") {
          num_period <- convert.rate.period(period)
          # Was genau wird hier berechnet?
          out[k] <- (prod( 
            (1 + rates/num_period)^(dt*num_period) )^(1/(T*num_period)) - 1
            ) * num_period
        } else if (method == "continuous") {
          out[k] <- sum( rates * dt )/T
        } else {
          stop(paste("ErrorIn::DynamicYieldCurve::getRateAt:: Method ", 
                     method, " not supported !!!"))
        }
      }
        
    }

    # replace NA values with 0 (happens in case yearFraction is 0)
    out[is.na(out)] <- 0
    return(out)
  })



##############################################################
#' Generic method to retrieve the rate(s) for (a) specific
#' tenor(s) from a \code{\link{DynamicYieldCurve}} object
#'
#' A yield curve (or more precisely spot rate curve) describes
#' the term structure of interest rates. The interest rates are calculated
#' with respect to a given (analysis) adte \code{ad} for money that is borrowed or
#' lend at t1>=ad until time t2>t1. 
#' The term or tenor is t2-t1.
#' Cf. below for details.
#' 
#' 
#' @param object An object of class \code{DynamicYieldCurve} for 
#'        which to return the yield for a given tenor
#'        
#' @param from (optional) Date(s) t1. Should be a \code{character} variable with 
#' format 'YYYY-MM-DD'. Is set to \code{ad} if missing.
#' 

#' @param to (optional, character) The date(s) t2. Should have the same format as \code{from}.
#' 
#' @param by (optional) The tenor(s). Should be for form 'nX' where 'n' is an integer
#' and 'X'=("D"|"W"|"M"|"Q"|"H"|"Y") for days, weeks, months, quarters, half-years 
#' and years, respectively. Either \code{to} or \code{by} must be defined.
#' 
#' @param ad (optional) The analysis date \code{ad} with respect to which the interest
#' rates are calculated. Should have the same format as \code{from}. 
#' If missing or \code{ad}=\code{to}, spot rates are calculated. 
#' If \code{ad}<\code{to}, forward rates are calculated.
#' 
#' 
#' @param Additional parameters:
#' 
#' \itemize{
#'  \item{"isDateEnd"}{(deprecated) logical indicating whether to is 
#'        of date (TRUE) or term (FALSE) format. Date format is
#'        'YYYY-MM-DDTXX' with 'XX'=00 for beginning of day, or
#'        24 for end of day, and term format is 'IP' with 'I' 
#'        an integer and 'P' indicating the period (D=days, 
#'        W=weeks, M=months, Q=quarters, H=halfyears, Y=years).
#'        Default is isDateEnd=FALSE.}
#'   \item{"refdate"} {(deprecated) an alternative name for \code{ad}}
#' }
#'
#' @return \code{numeric} The interest rates for the defined tenor(s)
#' 
#' @usage rates(object, from, to, by, ad, refdate, ...)
#' 
#' @details 
#' The 
#' \code{\link{DynamicYieldCurve}} object contains tenors with
#' associated spot interest rates. Rates can be retrieved for any
#' tenor by inter-/extrapolation.
#' 
#' For the forward rate at \code{ad} 
#'        between times t1 and t2, \code{to} refers to t2 and
#'        \code{from} refers to t1. Is a single value. If combined 
#'        with a vector for \code{to}, then \code{from} remains
#'        the same for all t2 defined in \code{to}.
#'        
#' @seealso \code{\link{discountFactors}}
#' 
#' @examples
#' yc <- DynamicYieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' 
#' rates(yc, by="1Y")  # 1-year spot rate
#' rates(yc, to="2016-01-01T00") # again, 1-year spot rate
#' rates(yc, by="1Y", from="2015-07-01T00")  # 1-year forward rate at 2015-07-01T00
#'
## @include
#' @export
#' @docType methods
#' @rdname rts-methods
#' @aliases rates, DynamicYieldCurve, charachter, missing-method
#' @aliases rates, DynamicYieldCurve, character, character-method
setGeneric(name = "rates",
           def = function(object, ...){
             standardGeneric("rates")
           })
# setGeneric(name = "rates",
#            def = function(object, to, from, ...){
#              standardGeneric("rates")
#            })

#' @export
#' @rdname rts-methods
#' @aliases rates, YieldCurve, character, missing-method
setMethod(f = "rates",
          signature = c("YieldCurve"),
          definition = function(object, 
                                from, to, by, ad, refdate,
                                # from=NULL, to=NULL, by=NULL, ad=NULL, refdate=NULL, 
                                ...){
            # browser()
            # if (missing(ad)) {
            #   ad <- NULL
            # }
            out <- rates(as.DynamicYieldCurve(object), 
                         from, to, by, ad, refdate,
                      # from, to, by, ad, refdate, 
                         ...)
            return(out)
          })


## @include
#' @export
#' @rdname rts-methods
#' @aliases rates, DynamicYieldCurve, character, missing-method
setMethod(f = "rates",
          signature = c("DynamicYieldCurve"),
          definition = function(object, from, to, by, ad, refdate, ...){
            # Comment:
            # 'ad' is the analysis date.
            #  Alternatively, 'refdate' can be used for backwards compatibility.
            if (missing(ad)) {
              ad <- NULL
            }
            if (missing(refdate)) {
              refdate <- ad
            }

            if (missing(from)) {
              if (is.null(refdate) && is.null(ad)) {
                from <- object$ReferenceDate
              } else {
                if (!is.null(ad)) {
                  from <- ad
                } else {
                  from <- refdate
                }
              }
            }
            
            if (missing(to)) {
              if (missing(by)) {
                stop("Either argument 'to' or 'by' must be defined.")
              } else {
                endDate <- shiftDates(from, by)
              }
            } else {
              if (missing(by)) {
                endDate <- to
              } else {
                stop("Only one of arguments 'to' and 'by' should be defined.")
              }
            }
            
            test.dates(endDate)
            test.dates(from)
            # test.dates(refdate)
            
            out <- getRateAt(object, from=from, to=endDate, refdate = refdate, ...)
            return(out)
            
          })

##############################################################
#' Generic method to retrieve discount factors for specific
#' tenor(s) from a \code{\link{YieldCurve}} or \code{\link{DynamicYieldCurve}} 
#' object.
#'
#' Discount factors for specified tenors are extracted from a 
#' \code{\link{YieldCurve}} or \code{\link{DynamicYieldCurve}} 
#' object according to the compounding method specified. 
#' 
#' @param object An object of class \code{\link{YieldCurve}} or 
#'        \code{DynamicYieldCurve} from which to extract the 
#'        discount factors.
#'        
#' @param to character reflecting the discounting period end date.
#'        Can be a single value or a vector of tenors.
#' 
#' @param from (optional) a character defining the discounting 
#'        period start date. If it is a single value combined 
#'        with a vector for 'end', then 'start' remains
#'        the same for all dates defined in 'end'.
#'  
#' @param by (optional)  
#' 
#' @param method
#' 
#' @param period
#' 
#' @param ad
#' 
#' @param refdate
#' 
#' @param ... Additional parameters to be passed.
#' \itemize{
#'  \item{"isDateEnd"} {(deprecated)logical indicating whether 'end' is 
#'        of date (TRUE) or term (FALSE) format. Date format is
#'        'YYYY-MM-DD'. Default is isDateEnd = FALSE.}
#' }
#' 
#' @return numeric vector of discount factors for the defined periods.
#' 
#' @usage discountFactors(object, to, from, method = "continuous", 
#' period = "Y", refdate = NULL, ...)
#' 
#' @seealso \code{\link{rates}}
#' 
#' @examples
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' yc <- DynamicYieldCurve(label = "YC_Prim",
#'                         ReferenceDate = "2015-01-01",
#'                         Tenors = tenors,
#'                         Rates = rates)
#' 
#' discountFactors(yc, by="1Y")
#' discountFactors(yc, to="2016-01-01", isDateEnd = TRUE)
#' discountFactors(yc, by="1Y", from="2015-07-01")
#'
#' @export
#' @docType methods
#' @rdname dfs-methods
## @aliases
setGeneric(name = "discountFactors",
           def = function(object, ...){
             standardGeneric("discountFactors")
           })


#' @export
#' @rdname dfs-methods
#' @aliases discountFactors, YieldCurve, character, character-method
setMethod(f = "discountFactors",
          signature = c("YieldCurve"), 
          definition = function(object, ...) {
            return(discountFactors(as.DynamicYieldCurve(object), ...))
          })


#' @export
#' @rdname dfs-methods
#' @aliases discountFactors, DynamicYieldCurve, character, character-method
setMethod(f = "discountFactors",
          signature = c("numeric"),
          definition = function(object, to, from, ...) {
            yc <- as.DynamicYieldCurve(MarketInterestRate(object, min(from, to)))
            return(discountFactors(yc, to=to, from=from, ...))
          })


## @include
#' @export
#' @rdname dfs-methods
#' @aliases discountFactors, DynamicYieldCurve, character, missing-method
setMethod(f = "discountFactors",
          signature = c("DynamicYieldCurve"),
          definition = function(object, to, from, by, 
                                method = "continuous", period = "Y", 
                                ad=NULL, refdate, ...){
            # To consider: 
            # Implementierung von Unterjährigen Zinsen bei Bruchteilen von Perioden

            # Comment:
            # 'ad' is the analysis date.
            #  Alternatively, 'refdate' can be used for backwards compatibility.
            if (missing(refdate)) {
              refdate <- ad
            }
            
            if (missing(from)) {
              if (is.null(refdate) && is.null(ad)) {
                from <- object$ReferenceDate
              } else {
                if (!is.null(ad)) {
                  from <- ad
                } else {
                  from <- refdate
                }
              }
            }
            
            # test dates and converts to date if needed
            if (missing(to)) {
              if (missing(by)) {
                stop("Either argument 'to' or 'by' must be defined.")
              } else {
                endDate <- tenors2dates(from, by)
              }
            } else {
              if (missing(by)) {
                endDate <- to
              } else {
                stop("Only one of arguments 'to' and 'by' should be defined.")
              }
            }
            test.dates(from)
            # err_testing <- tryCatch(test.dates(to), error = function(e) e)
            # if (any(class(err_testing) == "error")) {
            #   endDate <- tenors2dates(from, to)
            # } else {
            #   endDate <- to
            # }
            test.dates(endDate)
            
            # calcluate year fraction
            yearFraction <- yearFraction(from, endDate, object$DayCountConvention)
            
            # get the required rate from the yield curve
            rates <- getRateAt(object, from=from, to=endDate, method = method, 
                               period = period, refdate = refdate, ...)
            
            # return requested discount factors
            if (method == "linear") {
              return((1 + rates*abs(yearFraction))^sign(-yearFraction))
            } else if (method == "compound") {
              num_period <- convert.rate.period(period)
              return((1 + rates/num_period)^(-yearFraction*num_period))
            } else if (method == "continuous") {
              return(exp(-yearFraction * rates))
            } else {
              stop(paste("ErrorIn::discountFactors:: Method ", method, 
                         " not supported !!!"))
            }
          })

#' @export
setGeneric(name = "getRateSeries",
           def = function(object, startdate, enddate, ...){
             standardGeneric("getRateSeries")
           })
#' @export
setMethod(f = "getRateSeries",
          # Returns forward rate for start and end date. Deprecated. 
          signature = c("RiskFactor", "character", "character"),
          definition = function(
            object, startdate, enddate, frequency = "week", forward = "1M", ...){
            
            # check freq inputs are valid
            test.dates(startdate)
            test.dates(enddate)
            if (!frequency %in% c("day", "week", "month", "quarter", "year")) {
              stop(paste("ErrorIn::DynamicYieldCurve::getRateSeries::", 
                         "Frequency must be one of 'day', 'week', 'month', 'quarter' or 'year' !!!"))
            }
            
            # create date vector with frequency defined
            dt_seq <- as.character(seq.Date(as.Date(startdate) + 1, 
                                            as.Date(enddate), by = frequency) - 1)
            
            # calculate forward rates with forward time defined
            rates_seq <- rates(object, by=forward, from=dt_seq) # Changed. 'dt_seq' is date, not period
            
            # output in a timeSeries
            ts <- timeSeries(data = as.numeric(rates_seq),
                             charvec = dt_seq,
                             units = "Values")
            return(ts)
            })

## @include
#' @export
setMethod(f = "show", signature = c("DynamicYieldCurve"),
          definition = function(object){
            cat(paste0("Label: ", object$label,"\n"))
            cat(paste0("DayCountConvention: ", object$DayCountConvention,"\n"))
            curve <- object$Rates
            print("Curve:")
            print(curve)
            if (!is.null(body(object$FUN))) {
              cat("\nFUN:\n")
              print(object$FUN)
              # cat("fParams:\n")
              # print(object$fParams)
            }
          })

## @include
#' @export
setMethod(f = "names", signature = c("DynamicYieldCurve"),
          definition = function(x){
            return(names(x$getRefClass()$fields()))
          })


##############################################################
#' A Reference Class 
#' 
#' It should be generalized by using \code{splinefun}. 
#' In this case, defining a class has real value.
#' 
#' Note: A class of the same name is defined in 'YieldCurve.R' 
#' but only used here.
#' 
#' @export
Interpolator <- setRefClass(
  "Interpolator", 
  fields = list(xValues = "numeric", yValues = "numeric"),
  methods = list(
    initialize = function(...) {
      pars <- list(...)
                                
      all_fields <- names(getRefClass("Interpolator")$fields())
      pars_names <- names(pars)
      test_pars_names <- pars_names %in% all_fields
      if (!all(test_pars_names)) {
        stop(paste(
          "ErrorInDynamicYieldCurve::Interpolator:: Interpolator has no field called: ", 
          pars_names[!test_pars_names], "!!!"))
      }
                                
      if (length(pars$xValues) != length(pars$yValues)) {
        stop("ErrorInDynamicYieldCurve::Interpolator:: xValues and yValues must have same length !!!")
      }
      .self$xValues <- pars$xValues
      .self$yValues <- pars$yValues
    },
    getValueAt = function(x){
      xOut <- approx(xValues, yValues, x, method = "linear", rule = 2)
      return(xOut$y)
    }
))



## -----------------------------------------------------------------
## helper methods
# existing fields in the DynamicYieldCurve class
# ATTENTION: This is EXACT replica of a function in 'YieldCurve.R'
validYieldCurveFields <- function() {
  return(c("Rates", "Tenors", "ReferenceDate", "label",
           "DayCountConvention", "TenorDates", "FUN"
           # , "fParams")
           ))
}

# check if fields are valid
# ATTENTION: This is EXACT replica of a function in 'YieldCurve.R'
is.valid.yieldcurve.field <- function(x) {
  valid <- validYieldCurveFields()
  return(x %in% valid)
}

# convert character terms to dates relative to a refDate
tenors2dates <- function(refDate, tenors, frame=FALSE){
  
  # relativeDates <- c("")
  relativeDates <- matrix(NA, nrow=length(refDate),ncol=length(tenors))
  for (i in 1:length(tenors)) {
    count <- as.numeric(substr(tenors[i], 1, nchar(tenors[i])-1))
    switch(substr(tenors[i], nchar(tenors[i]), nchar(tenors[i])),
           "D" = {
             relativeDates[,i] <- as.character(ymd(refDate) %m+% days(count))
           },
           "W" =  {
             relativeDates[,i] <- as.character(ymd(refDate) %m+% weeks(count))
           },
           "M" = {
             relativeDates[,i] <- as.character(ymd(refDate) %m+% months(count))
           },
           "Q" = {
             quarter_count <- count * 3
             relativeDates[,i] <- as.character(ymd(refDate) %m+% months(quarter_count))
           },
           "H" = {
             halfyear_count <- count * 6
             relativeDates[,i] <- as.character(ymd(refDate) %m+% months(halfyear_count))
           },
           "Y" = {
             relativeDates[,i] <- as.character(ymd(refDate) %m+% years(count))
           }
    )
  }
  dimnames(relativeDates) <- list(refDate, tenors)
  
  if (!frame) {
    if (nrow(relativeDates)>1) {
      stop(paste("ErrorIn::tenors2dates::", 
                 "If function should return array, only one reference date is allowed !!!"))
    }
    out <- c(relativeDates)
  } else {
    out <- data.frame(relativeDates)
    colnames(out) <- dimnames(relativeDates)[[2]]
  }
  return(out)
}


shiftDates <- function(dates, shift) {
  # browser()
  # dates is a vector and shift is a scalar
  units <- substrRight(shift, 1)
  counts <- gsub('.{1}$', '', shift)
  dCF <- dayCountFcts[units]
  counts <- as.numeric(counts)
  out = character()
  for (i in 1:length(units)) {
    out <- rbind(out,
                 as.character(ymd(dates) %m+% do.call(dCF[i],  list(counts[i])))
    )
  }
  if (nrow(out)==1 || ncol(out)==1 ) {
    out <- as.character(out)
  }
  out
}


test.dates <- function(date) {
  tryCatch({
    as.Date(date)
  }, error = function(e) {
    stop("ErrorIn::DynamicYieldCurve Dates are not valid !!!")
  })
}


convert.rate.period <- function(period) {
  allowed_periods <- c(1, 2, 4, 12, 52, 360)
  names(allowed_periods) <- c("Y", "H", "Q", "M", "W","D")
  
  if(period %in% names(allowed_periods)) {
    period_num <- allowed_periods[[period]]
  } else {
    stop(paste("ErrorIn::discountFactors:: ", period, 
               " is not a valid interest rate period !!!", sep=" "))
  }
  return(period_num)
}


as.DynamicYieldCurve <- function(yc){
  dyn_yc <- DynamicYieldCurve()
  rts <- setNames(data.frame(t(yc$Rates)), yc$Tenors)
  rownames(rts) <- yc$ReferenceDate
  attributes <- list(label = yc$label,
                     ReferenceDate = yc$ReferenceDate,
                     Rates = rts,
                     DayCountConvention = yc$DayCountConvention,
                     FUN = yc$FUN)
  set(dyn_yc, attributes)
  return(dyn_yc)
}

