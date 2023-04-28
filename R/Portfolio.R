# Portfolio.R  FEMS dev code by Vigan Hamzai Nov 2022
# Edits/subset of Portfolio.R in main FEMS branch
# Licensing and Copyright notices from there
# *********************************************************************
# class Portfolio
# *************************************
#' class Portfolio
#'
#' A Portfolio consists of a list of contracts such as
#' @include ContractType.R
#' @import methods
#' @importFrom methods new
#' @export Portfolio
#' @exportClass Portfolio
#' 
#' @field contracts  List of contracts, class=ContractType, in the portfolio.
#' 
setRefClass("Portfolio",
            fields = list(
              contracts = "list"   # contracts are instances of ContractType
            ))

# **************************************
# constructors Portfolio(...) for a portfolio object
# *************************************
#' Portfolio < >  -  generic function definition 
#'
#' Defines generic S4 constructor method on class Portfolio
#' @param  contract   S4 reference Class=ContractType, a contract to include. 
#' @param  ...        Not used
#' 
setGeneric(name = "Portfolio",
           def = function(contract, ...){
             standardGeneric("Portfolio")
           })

#' Portfolio ( )  - no parameters instance of Portfolio< > 
#' 
#' Creates an empty Portfolio object with no attributes initialized. 
#' @return  S4 reference with class=Portfolio and no attributes initialized.
#' 
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
            return(new("Portfolio"))
          })

# ************************************************************
# samplePortfolio(contractDataFileName)
# ************************************************************
#' samplePortFolio
#'
#' samplePortfolio(cdfn) takes as input a contracts-data-filepath 
#'   reads this data and returns an initialized
#'   Portfolio object with contracts and from this csv file.
#' @param cdfn      character string -  a contract-data-filepath
#' @param type      character string - defines if operations or contracts
#'
#' @return   Portfolio s4 object initialized with the data from the input files
#' @export
#' @include utils.R
#' @importFrom utils read.csv
#'
samplePortfolio <- function(cdfn, type) {
  ptf <- Portfolio()  # create portfolio object no attributes set
  
  if(type == "contracts"){
    ptf$contracts <- contracts_df2list(contractFile2dataframe(cdfn))
  }else if (type == "operations"){
    ptf$contracts <- operations_df2list(operationFile2dataframe(cdfn))
  }else{
    stop("Portfolio: No known type")
  }
  
  return(ptf)
}


#' getPortfolioAsDataFrame <ptf>     Generic method definition
#'
#'   Defines a generic method on S4 Class Portfolio to transform
#'   portfolio ptf given as input parameter to a data.frame
#'   with reduced attributes.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts
#' @return      A data.frame with reduced attributes for given Portfolio
#' 
setGeneric(name = "getPortfolioAsDataFrame",
           def = function(ptf) standardGeneric("getPortfolioAsDataFrame"))


#' getPortfolioAsDataFrame(ptf)
#'
#'   Defines a generic method on S4 Class Portfolio to transform
#'   portfolio ptf given as input parameter to a data.frame
#'   with reduced attributes.
#'   
#'   Attributes in data.frame:
#'    - ContractID
#'    - ContractRole
#'    - StatusDate
#'    - Currency
#'    - NotionalPrincipal
#'    - NominalInterestRate
#'    - SpreadRate
#'    - ContractDealDate
#'    - InitialExchangeDate
#'    - MaturityDate
#'    - MarketObjectCode
#'
#' @param ptf   S4 ref to class=Portfolio object with list of contracts
#' @return      A data.frame with reduced attributes for given Portfolio
#' @export
#'
setMethod ( f = "getPortfolioAsDataFrame",  signature = c("Portfolio"),
            definition = function(ptf) {
              
              if(length(ptf$contracts) > 0){
                crid <- 1:length(ptf$contracts)
                contracts_df <-data.frame(crid)
                
                for(i in 1:length(ptf$contracts)){
                  
                  if(!ptf$contracts[[i]]$contractTerms$contractType %in% c("Investments", "OperationalCF")){
                    Contract_Field_Names <- c("node", "contractID", "contractType", "contractRole", "currency", "notionalPrincipal",
                                              "nominalInterestRate", "initialExchangeDate", "maturityDate")
                  }else{
                    
                    Contract_Field_Names <- c("node","contractID", "contractType", "contractRole", "currency", "notionalPrincipal", 
                                              "initialExchangeDate", "initialExchangeDate", "maturityDate", )
                  }
                  
                  for(crfield in Contract_Field_Names) {
                    contracts_df[crfield] <- unlist(sapply(ptf$contracts,
                                                           function(ct){ct$contractTerms[[crfield]]}))
                  }
                }
                contracts_df <- subset(contracts_df, select = -crid)
              }else{
                Contract_Field_Names <- c("node", "contractID", "contractType", "contractRole", "currency", "notionalPrincipal",
                                          "nominalInterestRate", "initialExchangeDate", "maturityDate")
                contracts_df <- data.frame(matrix(ncol = length(Contract_Field_Names), nrow = 0))
                colnames(contracts_df) <- Contract_Field_Names
              }

              return(contracts_df)
              
            } )
