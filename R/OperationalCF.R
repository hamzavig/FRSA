## -----------------------------------------------------------------
#' OperationalCF Contract class definition
#' 
#' An operational cash flows contract represents any operational activity 
#' in monetary units within an organization. 
#' 
#' @param pattern A function evaluating the pattern of generated cash flows. 
#' 
#' @param args The list of arguments used when evaluating the pattern
#' 
#' @usage OperationalCF(ContractID, pattern, args, ...)
#' 
#' @include FEMSContract.R
#' @import methods
#' @importFrom methods new
#' 
setRefClass("OperationalCF",
            contains = "FEMSContract")

setGeneric(name = "OperationalCF",
           def = function(...){
             standardGeneric("OperationalCF")
           })


setMethod(f = "OperationalCF", signature = c(),
          definition = function(...){
            object = new("OperationalCF")
            object$contractTerms<- list()
            object$pattern <- function()
            object$args <- list()
            return(object)
          })
