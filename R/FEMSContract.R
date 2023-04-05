# FEMSContract definition
##############################################################
#' FEMSContract is  Reference Class parent to all FEMS Contract Types
#'
#' This class is only used in an abstract sense in that it is
#' not intended to create instances of it. It's use is to
#' serve as parent to various implementations of FEMS CTs (Operations) as
#' a means for designing method-inheritance.'
#'
#' @import methods
#' @importFrom methods new
#' @include ContractABC.R
#' @include ContractModel.R
#' 
setRefClass("FEMSContract",
            contains = "ContractABC",
            fields = list(
              contractTerms = "list",
              pattern = "function",
              args = "list"
            ))

setGeneric(name = "FEMSCT",
           def = function(contract_name){
             standardGeneric("FEMSCT")
           })

setMethod(f = "FEMSCT", signature = c("character"),
          definition = function(contract_name) {
            return(new(contract_name))
          })

setMethod(f = "set", signature = c("FEMSContract", "list"),
          definition = function(object, what){
            for (i in 1:length(what)) {
              object$contractTerms[names(what[i])] <- what[[i]]
            }
          })