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

#' Portfolio("ContractType")  Constructs Portfolio containing a single contract.
#' 
#' This instance of the generic Portfolio< > method takes a reference to a 
#' contract as its input parameter and returns a portfolio with no defined risk 
#' Scenario and this single contract as its contents
#' @param contract  S4 reference class=ContractType
#' @return   S4 reference class=Portfolio, initialized attributes
#' 
setMethod(f = "Portfolio", signature = "ContractType",
          definition = function (contract) {
            ptf <- Portfolio()
            ptf$contracts = list(contract)
            return(ptf)
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


#' getContractIDs  <ptf>     Generic method definition
#'
#' Defines a generic method on S4 Class Portfolio. Returns a vector with the
#' contractIDs of all the contracts in the portfolio.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts.
#' @return      A vector of character string contractIDs
#' 
setGeneric(name = "getContractIDs",
           def = function(ptf) standardGeneric("getContractIDs"))

#' getContractIDs
#'
#' getContractIDs(ptf) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts. It returns a vector of character string
#'     contractID of the contracts in the portfolio.
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @return       Vector of character string contractIDs -  one for each contract
#'               in the portfolio
#' @export
#'
setMethod (f = "getContractIDs", signature = c("Portfolio") ,
           definition = function(ptf){
             cids <- sapply(ptf$contracts, getCIDfromContract)
             return(cids)
           })


#' getContract <ptf, cid >     Generic method definition
#'
#'   Defines a generic method on S4 Class Portfolio also taking character string
#'   contractID as its second input. Returns an S4 reference to an object of
#'   Class=ContractType if the input contractID matches a contract in the
#'   Portfolio and NULL if it does not.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts
#' @param cid   A character string contractID.
#' @return      An S4 Reference to a portfolio Object if cid is matched or NULL
#' 
setGeneric(name = "getContract",
           def = function(ptf, cid ) standardGeneric("getContract"))


#' getContract(ptf, cid)
#'
#' getContract(ptf, cid) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts and a character string contractID. The
#'     returns either an S4 ref to a Class=ContractType contract object whose
#'     contractID the input cid string OR NULL if there is no matching contract
#'     in the portfolio.
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @param cid    character - a contractID string to be matched
#' @return       EITHER an S4 Ref to a class=ContractType object with this input
#'                      cid as its its contractID
#'               OR NULL if no such match exists
#' @export
#'
setMethod ( f = "getContract",  signature = c("Portfolio", "character"),
            definition = function(ptf, cid) {
              cl <- ptf$contracts[
                sapply(ptf$contracts, function(cntr){
                  getCIDfromContract(cntr) == cid
                }) ]
              if (length(cl) == 1)  cntr_out <- cl[[1]]
              else  cntr_out = NULL
              return(cntr_out)
            } )


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
              
              crid <- 1:length(ptf$contracts)
              contracts_df <-data.frame(crid)
              
              for(i in 1:length(ptf$contracts)){
                
                if(!ptf$contracts[[i]]$contractTerms$contractType %in% c("Investments", "OperationalCF")){
                  Contract_Field_Names <- c("contractID", "contractRole", "statusDate", "currency", "notionalPrincipal",
                                            "nominalInterestRate", "contractDealDate", "initialExchangeDate","maturityDate")
                }else{
                  
                  Contract_Field_Names <- c("contractID", "contractType", "contractRole", "currency", "initialExchangeDate","maturityDate", "notionalPrincipal")
                }
                
                for(crfield in Contract_Field_Names) {
                  contracts_df[crfield] <- unlist(sapply(ptf$contracts,
                                                         function(ct){ct$contractTerms[[crfield]]}))
                }
              }
              contracts_df <- subset(contracts_df, select = -crid)
              return(contracts_df)
            } )


#' mergePortfolios <ptf1, ptf2>     Generic method definition
#'
#'   Defines a generic method on S4 Class Portfolio to merge
#'   two portfolios ptf1 and ptf2 given as input parameter into one Portfolio 
#'   holding all the required of an institution.
#'
#' @param ptf1   S4 ref to class=Portfolio object with list of contracts
#' @param ptf2   S4 ref to class=Portfolio object with list of contracts
#' @return       S4 ref to class=Portfolio object with merged list of contracts
#' 
setGeneric(name = "mergePortfolios",
           def = function(ptf1, ptf2) standardGeneric("mergePortfolios"))


#' mergePortfolios(ptf1, ptf2)
#'
#'   mergePortfolios(ptf1, ptf2) takes as input two S4 Class Portfolio to merge
#'   them into one Portfolio holding all the required of an institution.
#'
#' @param ptf1   S4 ref to class=Portfolio object with list of contracts
#' @param ptf2   S4 ref to class=Portfolio object with list of contracts
#' @return       S4 ref to class=Portfolio object with merged list of contracts
#' @export
#'
setMethod ( f = "mergePortfolios",  signature = c("Portfolio", "Portfolio"),
            definition = function(ptf1, ptf2) {
              ptf <- Portfolio()
              ptf$contracts <- append(ptf$contracts, ptf1$contracts)
              ptf$contracts <- append(ptf$contracts, ptf2$contracts)
              return(ptf)
            } )




