##############################################################
#' A Reference Class extending \code{\link{RiskFactor}} class
#' and representing a default curve risk factor
#' 
#' Default curves define a class of market risk factors 
#' that directly affect future cash flows arising from a financial instrument
#' 
#' @seealso \code{\link{RiskFactor}}
#' 
#' @include RiskFactor.R 
#' @export
#' @rdname rf-classes
setRefClass("DefaultCurve", 
            contains = "RiskFactor",
            fields = list(ReferenceDate = "character",
                          Tenors = "character",
                          Rates = "numeric",
                          TenorDates = "character",
                          DayCountConvention = "character",
                          FUN = "function"
            ))


##############################################################
#' \code{DefaultCurve}-class constructor
#'
#' Create an instance of \code{DefaultCurve} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM.
#' 
#' @param ...
#'
#' @return An object of class \code{DefaultCurve} 
#'          containing the reference to the Java object
#' 
#' @seealso \code{\link{YieldCurve}}
#'
#' @export
#' @docType methods
#' @rdname dc-methods
#' @aliases DefaultCurve-method
#' 
setGeneric(name = "DefaultCurve",
           def = function(...){
             standardGeneric("DefaultCurve")
           })


#' @export
#' @rdname dc-methods
#' 
setMethod(f = "DefaultCurve",signature = c(),
          definition = function(...){
            
            pars <- list(...)
            
            # fill fields with NULL values
            fill_fields <- list()
            fill_fields$label <- "Generic_Default_Curve"
            fill_fields$ReferenceDate <- as.character(today())
            fill_fields$Tenors <- "0M"
            fill_fields$Rates <- Inf
            fill_fields$DayCountConvention <- "30E360"
            fill_fields$FUN <- NULL
            
            if (length(names(pars)) != 0) {
              
              # check if some fields are incorrectly specified!
              all_fields <- names(getRefClass("DefaultCurve")$fields())
              pars_names <- names(pars)
              test_pars_names <- pars_names %in% all_fields
              if (!all(test_pars_names)) {
                stop(paste("ErrorInDefaultCurve:: Default Curve has no field called: ", 
                           pars_names[!test_pars_names], "!!!"))
              }
              
              # check if necessary fields are missing and set the provide field names
              if (length(names(pars)) > 1) {
                if (!all(c("ReferenceDate","Tenors","Rates") %in% names(pars))) {
                  stop(paste(
                    "ErrorInDefaultCurve:: If any, all of the following fields have to be provided: "
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
            
            dc <- new("DefaultCurve")
            for (nms in names(fill_fields)) {
              dc[[nms]] <- fill_fields[[nms]]
            }
            
            test.dates(dc$ReferenceDate)
            dc$TenorDates <- tenors2dates(dc$ReferenceDate, dc$Tenors)
            return(dc)
          })


#' @export
#' @rdname set-methods
#' 
setMethod(f = "set", signature = c("DefaultCurve", "list"),
          definition = function(object, what, ...){
            
            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.defaultcurve.field(i)) {
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
                warning(paste("ErrorInDefaultCurve:: Field ", i, 
                              " does not exist, cannot assign value!", sep = ""))
              }
            }
            if (length(object$Tenors) != length(object$Rates)) {
              stop("ErrorInDefaultCurve::set:: Rates must have same length as Tenors")
            }
          })


#' @export
#' @rdname get-methods
#' @aliases get,RiskFactorConnector,character-method
#' 
setMethod(f = "get", signature = c("DefaultCurve", "character"),
          definition = function(object, what, ...){
            out <- list()
            if (length(what) == 1 && tolower(what) == "all") {
              what <- names(object$getRefClass()$fields())
            }
            for (i in what) {
              if (is.valid.defaultcurve.field(i)) {
                out[[i]] <- switch(i,
                                   label = object$label,
                                   ReferenceDate = object$ReferenceDate,
                                   Tenors = object$Tenors,
                                   Rates = object$Rates,
                                   DayCountConvention = object$DayCountConvention,
                                   TenorDates = object$TenorDates
                )
              } else {
                warning(paste("ErrorInDefaultCurve::get:: Field ", i, 
                              " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })


#' @export
#' 
setMethod(f = "show", signature = c("DefaultCurve"),
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
            }
          })


#' @export
#' 
setMethod(f = "names", signature = c("DefaultCurve"),
          definition = function(x){
            return(names(x$getRefClass()$fields()))
          })


################################################################################
#' Interpolate the DefaultCurve and get the rates for specified date range
#' from until to
#' 
#' @export
setGeneric(name = "getRatesAsSeries",
           def = function(object, dates, ...){
             standardGeneric("getRatesAsSeries")
           })


#' @export
setMethod(f = "getRatesAsSeries", signature = c("DefaultCurve", "character"),
  definition = function(object, dates, refdate = NULL, ...){
    
    refdate <- min(c(dates))

    # check if any of the dates are before first reference date of default curve
    if ((refdate < object$ReferenceDate)) {
      stop(paste("ErrorIn::DefaultCurve::getRatesAsSeries::", 
                 "No Default can be calculated before first ReferenceDate of the DefaultCurve!!!"))
    }
    
    # get necessary year fractions
    t <- yearFraction(object$ReferenceDate,
                      dates,
                      object$DayCountConvention)

    interpolator <- Interpolator(xValues = yearFraction(object$ReferenceDate, 
                                                        as.character(object$TenorDates), 
                                                        object$DayCountConvention), 
                                 yValues = as.numeric(object$Rates))
        
    # get rates from interpolation
    s <- interpolator$getValueAt(t)
    
    rates.ts <- timeSeries(data = s,
                           charvec = dates,
                           units = "defaultRates")
    
    return(rates.ts)
  })

## -----------------------------------------------------------------
## helper methods

validDefaultCurveFields <- function() {
  return(c("Rates", "Tenors", "ReferenceDate", "label",
           "DayCountConvention", "TenorDates", "FUN"))
}

# check if fields are valid
is.valid.defaultcurve.field <- function(x) {
  valid <- validDefaultCurveFields()
  return(x %in% valid)
}


# convert character terms to dates relative to a refDate
tenors2dates <- function(refDate, tenors, frame=FALSE){
  
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


test.dates <- function(date) {
  tryCatch({
    as.Date(date)
  }, error = function(e) {
    stop("ErrorIn::DefaultCurve Dates are not valid !!!")
  })
}



##############################################################
#' A Reference Class 
#' 
#' It should be generalized by using \code{splinefun}. 
#' In this case, defining a class has real value.
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
          "ErrorInDefaultCurve::Interpolator:: Interpolator has no field called: ", 
          pars_names[!test_pars_names], "!!!"))
      }
      
      if (length(pars$xValues) != length(pars$yValues)) {
        stop("ErrorInDefaultCurve::Interpolator:: xValues and yValues must have same length !!!")
      }
      .self$xValues <- pars$xValues
      .self$yValues <- pars$yValues
    },
    getValueAt = function(x){
      xOut <- approx(xValues, yValues, x, method = "linear", rule = 2)
      return(xOut$y)
    }
  )
)
