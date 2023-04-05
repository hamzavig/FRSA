#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Plot a \link{YieldCurve, DefaultCurve}
#'
#' Create a graphical representation of a \link{YieldCurve}-
#' object or a \link{DefaultCurve}-object
#' 
#' @param object The \link{YieldCurve, DefaultCurve} object to plot
#' 
#' @seealso \link{YieldCurve, DefaultCurve},\link{plot}
#' 
#' @examples
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' plot(yc)
#' 

#' @include YieldCurve.R
#' @export
#' @docType methods
#' @rdname plt-methods
setMethod("plot", signature("YieldCurve", "missing"),
          definition = function(x, y, ...){
            # extract information from yield curve
            name = get(x, "label")
            t0 = get(x, "ReferenceDate")
            tenors = get(x, "Tenors")
            rates = get(x, "Rates")
            # construct time-axis
            x.axis = timeDate(substring(t0,1,10))
            for(t in tenors) {
              x.axis = c(x.axis,
                         timeSequence(substring(t0, 1, 10),
                                      length.out=2, by=cycle.to.by(t))[2]
                         )
            }
            
            # get y-lim
            ylim = range(c(0, rates))
            if(!missing(y)) {
              ylim = range(c(ylim, range(y[,tenors])))
            }
            # plot structure
            plot(x=as.Date(x.axis), y=c(0, rates), xaxt="n", type="n",
                 xlab="Tenors", ylim=ylim, ylab="Rates", main=name)
            
            # plot shifts if exist
            if(!missing(y)) {
              for(i in 1:nrow(y)) {
                lines(x=as.Date(x.axis), 
                      y=c(0, as.numeric(rates + shock[i, tenors])),
                      col="gray")
              }
            }
            
            # plot curve
            pos = as.Date(x.axis)
            lbl = c("t0", tenors)
            for (i in 1:length(pos)) {
              axis(1, at=pos[i], labels=lbl[i], tick=TRUE)
            }
            lines(x=as.Date(x.axis[-1]), y=rates, lwd=2)
            
          })

#' @include DefaultCurve.R
#' @export
#' @docType methods
#' @rdname plt-methods
setMethod("plot", signature("DefaultCurve", "missing"),
          definition = function(x, y, ...){
            # extract information from yield curve
            name = get(x, "label")
            t0 = get(x, "ReferenceDate")
            tenors = get(x, "Tenors")
            rates = get(x, "Rates")
            # construct time-axis
            x.axis = timeDate(substring(t0,1,10))
            for(t in tenors) {
              x.axis = c(x.axis,
                         timeSequence(substring(t0, 1, 10),
                                      length.out=2, by=cycle.to.by(t))[2]
              )
            }
            
            # get y-lim
            ylim = range(c(0, rates))
            if(!missing(y)) {
              ylim = range(c(ylim, range(y[,tenors])))
            }
            # plot structure
            plot(x=as.Date(x.axis), y=c(0, rates), xaxt="n", type="n",
                 xlab="Tenors", ylim=ylim, ylab="Rates", main=name)
            
            # plot shifts if exist
            if(!missing(y)) {
              for(i in 1:nrow(y)) {
                lines(x=as.Date(x.axis), 
                      y=c(0, as.numeric(rates + shock[i, tenors])),
                      col="gray")
              }
            }
            
            # plot curve
            pos = as.Date(x.axis)
            lbl = c("t0", tenors)
            for (i in 1:length(pos)) {
              axis(1, at=pos[i], labels=lbl[i], tick=TRUE)
            }
            lines(x=as.Date(x.axis[-1]), y=rates, lwd=2)
            
          })

cycle.to.by <- function(x) {
  x = gsub("+", "", x)
  x = gsub("-", "", x)
  counter = substring(x, 1, nchar(x)-1)
  unit.char = substring(x, nchar(x), nchar(x))
  unit = switch(unit.char,
                Y="years",
                Q="months",
                M="months",
                W="weeks",
                D="days",
                "unknown")
  mult = switch(unit.char,
                Y=1,
                Q=3,
                M=1,
                W=1,
                D=1,
                "unknown")
  return(paste(mult*as.integer(counter), unit, sep=" "))
}


##############################################################
#' Plot an index-like \link{RiskFactor}
#'
#' Create a graphical representation of an index-like 
#' \link{RiskFactor}-object.
#' 
#' @param object The \link{RiskFactor} object to plot
#' 
#' @seealso \link{ReferenceIndex},\link{plot}
#' 
#' @examples
#' idx <- Index()
#' times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", "2018-01-01T00",
#'            "2019-01-01T00")
#' values <- c(100, 110, 120, 130, 140)
#' set(idx, what=list(
#'   MarketObjectCode = "IND_CPI_EA",
#'   Data=list(Dates=times,Values=values)))
#' plot(idx)
#' 
#' @include RiskFactor.R
#' @export
#' @docType methods
#' @rdname plt-methods
setMethod("plot", signature("RiskFactor"),
          definition = function(x, y, ...){
            # extract information from yield curve
            name=get(x, "label")
            times=get(x, "Dates")
            values=get(x, "Values")
            
            # plot risk factor
            plot(x=as.Date(times),y=values,
                 xlab="Times",ylab="Values",main=name,
                 type="l")
          })