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

setGeneric(name = "default", def = function(object, defaults, from, rawCtrs, ...){
  standardGeneric("default")
})

#' @rdname def-methods
#' @export
#' 
setGeneric(name = "generateDefaultContracts", def = function(object, defaults, from, ctr, ...){
  standardGeneric("generateDefaultContracts")
})


#' @include utils.R
#' @rdname def-methods
#' @export
#' 
setMethod(f = "generateDefaultContracts", signature = c("ContractType", "list", "character", "data.frame"),
          definition = function(object, defaults, from, ctr){
            
            ctrInitialExchangeDate <- object$contractTerms$initialExchangeDate
            ctrMaturityDate <- object$contractTerms$maturityDate
            ctrCounterParty <- object$contractTerms$legalEntityIDCounterparty
            
            dcLabels <- c()
            for(i in 1:length(defaults)) dcLabels <- c(dcLabels, defaults[[i]]$label)
            dcIdx <- which(dcLabels==ctrCounterParty)
            
            defaultCurve <- defaults[[dcIdx]]
            defaultDates <- as.character(timeSequence(from = from, to = ctrMaturityDate, by = "1 years"))
            defaultRates <- getRatesAsSeries(defaultCurve, defaultDates)
            
            paymentsPassed <- yearFraction(ctrInitialExchangeDate, from, "30E360")
            paymentsPending <- yearFraction(from, ctrMaturityDate, "30E360")
            paymentsTotal <- paymentsPassed + paymentsPending
            
            recoveryRate <- paymentsPassed/paymentsTotal
            defaultGivenRisk <- (1-recoveryRate)
            
            premiumDiscount <- defaultGivenRisk*defaultRates
            
            defCtrs <- list()

            for(i in 1:(length(defaultDates)-1)){
              
              def <- ctr
              
              def[1,"initialExchangeDate"] <- defaultDates[i]
              def[1,"contractRole"] <- "RPL"
              def[1,"statusDate"] <- defaultDates[i]
              def[1,"contractDealDate"] <- defaultDates[i]
              def[1,"premiumDiscountAtIED"] <- -(ctr[1,"notionalPrincipal"] * premiumDiscount[i])
              def[1,"notionalPrincipal"] <- ctr[1,"notionalPrincipal"] * defaultRates[i]
              
              defCtr <- contracts_df2list(def)
              defCtrs <- append(defCtrs, defCtr[[1]])
              
            }
            
            return(defCtrs)
            
          })


