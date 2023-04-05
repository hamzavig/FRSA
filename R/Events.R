#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the events for a \code{ContractType}
#'
#' Method generating the \code{\link{EventSeries}} of a contract, cf. section Details.
#'
#' @param object The \code{ContractType} for which to derive the events
#'
#' @param ad The analysis date as per which all future events are to be derived
#'
#' @param model (optional) The \code{RiskFactorConnector} conditional to which events are computed
#'  
#' @return A \code{EventSeries} object containing the resulting events
#'
#' @details 
#' The events of a contract are directly derived from the legal obligations 
#' defined therein. Events mark the exchange of a cash flow among the parties
#' involved in the contract or changes in the inner states of the contract
#' affecting cash flows to be exchanged in future. 
#' As such, \code{events} methods generates the
#' raw results that are the input for many financial analyses.
#' 
#' @seealso \link{ContractType}, \link{EventSeries}, \link{RiskFactorConnector}
#' 
#' @export
#' @docType methods
#' @rdname ev-methods

setGeneric(name = "events", def = function(object, processor, riskFactors, ...){
  standardGeneric("events")
  
})
