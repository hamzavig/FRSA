##############################################################
#' Default
#'
#' @param object object The \code{Institution}-object for which the contracts indicate a default risk
#'
#' @param defaults A list representing the default curves for the different economic sectors
#' 
#' @param from A character defining a date for the default simulation
#' 
#' @export
#' @docType methods
#' @rdname def-methods

setGeneric(name = "default", def = function(object, defaults, from, recoveryRate, ...){
  standardGeneric("default")
})

#' @rdname def-methods
#' @export
#' 
setGeneric(name = "generateDefaultContracts", def = function(object, defaults, from, ctr, recoveryRate, ...){
  standardGeneric("generateDefaultContracts")
})


#' @include utils.R
#' @rdname def-methods
#' @export
#' 
setMethod(f = "generateDefaultContracts", signature = c("ContractType", "list", "character", "data.frame", "numeric"),
          definition = function(object, defaults, from, ctr, recoveryRate){
            
            ctrInitialExchangeDate <- object$contractTerms$initialExchangeDate
            ctrMaturityDate <- object$contractTerms$maturityDate
            ctrCounterParty <- object$contractTerms$legalEntityIDCounterparty
            
            dcLabels <- c()
            for(i in 1:length(defaults)) dcLabels <- c(dcLabels, defaults[[i]]$label)
            dcIdx <- which(dcLabels==ctrCounterParty)
            
            defaultCurve <- defaults[[dcIdx]]
            defaultDates <- as.character(timeSequence(from = from, to = ctrMaturityDate, by = "1 years"))
            defaultRates <- getRatesAsSeries(defaultCurve, defaultDates)

            defaultGivenRisk <- (1-recoveryRate)
            
            premiumDiscount <- defaultGivenRisk*defaultRates
            
            defCtrs <- list()

            for(i in 1:(length(defaultDates)-1)){
              
              def <- ctr
              
              def[1,"initialExchangeDate"] <- defaultDates[i]
              def[1,"contractRole"] <- "RPL"
              def[1,"statusDate"] <- defaultDates[i]
              def[1,"contractDealDate"] <- defaultDates[i]
              def[1,"premiumDiscountAtIED"] <- -(as.numeric(ctr[1,"notionalPrincipal"]) * premiumDiscount[i])
              def[1,"notionalPrincipal"] <- as.numeric(ctr[1,"notionalPrincipal"]) * defaultRates[i]
              
              defCtr <- contracts_df2list(def)
              defCtrs <- append(defCtrs, defCtr[[1]])
              
            }
            
            return(defCtrs)
            
          })


