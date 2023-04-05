## -----------------------------------------------------------------
#' Investments Contract class definition
#' 
#' This contract represents the financial side of a real investment. 
#' In consists of an initial negative cash flow, a write-off period and a 
#' final positive cash flow for the salvage value.
#' 
#' @param pattern A function evaluating the pattern of generated investments. 
#' 
#' @param args The list of arguments used when evaluating the pattern
#' 
#' @usage Investments(ContractID, pattern, args, ...)
#' 
#' @details The \code{pattern} describes the value of the initial investment 
#' and the write-off pattern as \code{\link{timeSeries}} object. If the write-off
#' does not go down to zere the remaining value is interpreted as salvage value.
#' This is represented by a last event of type MD with a cash inflow of this
#' amount so that the \code{\link{EventSeries}} ends with a nominal value of zero.  
#' 
#' @include FEMSContract.R
#' @import methods
#' @importFrom methods new
#' 
setRefClass("Investments",
            contains = "FEMSContract")

setGeneric(name = "Investments",
           def = function(...){
             standardGeneric("Investments")
           })

setMethod(f = "Investments",signature = c(),
          definition = function(...){
            object = new("Investments")
            object$contractTerms<- list()
            object$pattern <- function()
            object$args <- list()
            return(object)
          })
