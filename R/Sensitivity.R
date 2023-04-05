##############################################################
#' Derive the sensitivity of a contract's value with respect
#' to a change in risk factors
#'
#' Different sensitivity-concepts can be derived from a financial
#' instrument or its analytical value, respectively. Currentently, 
#' these are only the Fisher-Weil for Maturity-contracts.
#' 
#' Fisher-Weil Duration is a measure for the sensitivity of the net present
#' value of a Maturity instrument to a parallel Shift in the related
#' yield curve. This measure is based on the discounted cash flows of the 
#' instrument.
#'
#' @param object object \code{Node}
#' 
#' @param yield object \code{YieldCurve}
#'
#' @return A \code{numeric} object giving the sensitivity
#' 
#' @export
#' @docType methods
#' @rdname sen-methods
#' 
setGeneric(name = "sensitivity", 
           def = function(object, yield, ...) {
  standardGeneric("sensitivity")
})

