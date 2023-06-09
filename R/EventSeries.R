# *************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
# *************************************************************

# *************************************************************
#' S4 class EventSeries holds a dataframe with the cashflow events generated for
#' a single Actus Contract, along with the contractID, contractType, statusDate
#' contructors: EventSeries()  EventSeries(<Contract>,<rfs>) 2B exported
#' @import  methods
#' @import  jsonlite
#' @import  httr
#' @include RiskFactor.R
#' @include Portfolio.R
#' @include ContractType.R
#' @include FEMSContract.R
#' @include Investments.R
#' @include OperationalCF.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include Events.R
#' @include utils.R
#' @export EventSeries
#' @exportClass EventSeries
#' @rdname EventSeries
#' 
setRefClass("EventSeries",
            fields = list(
              events_df = "data.frame",
              contractID = "character",
              contractType = "character",  # short form e.g. 'PAM'
              statusDate = "character",    # text yyyy-mm-dd
              riskFactors = "list")
            )

setGeneric(name = "EventSeries",
           def = function(object, processor, riskFactors){
             standardGeneric("EventSeries")
           })

setMethod(f = "EventSeries", signature = c(),
          definition = function(){
            return(new("EventSeries"))
          })

setMethod(f = "EventSeries", signature = c("ContractType", "character", "RiskFactorConnector"),
          definition = function(object, processor, riskFactors){
            
            # cast contract as list of list
            contracts <- list(object)
            
            # pass serverURL since unification of EventSeries this variable is named processor
            serverURL <- processor
            
            # prepare list in necessary structure to pass to JSON generator
            contractDefs <- lapply(contracts,preJcontract)
            
            if("marketObjectCodeOfRateReset" %in% names(contracts[[1]]$contractTerms)
               && contracts[[1]]$contractTerms$marketObjectCodeOfRateReset > ''){
              
              moc <- contracts[[1]]$contractTerms$marketObjectCodeOfRateReset
              factor <- riskFactors$riskfactors[[moc]]
              anchor_dt <- contracts[[1]]$contractTerms$cycleAnchorDateOfRateReset
              cycle <- contracts[[1]]$contractTerms$cycleOfRateReset
              mat <- contracts[[1]]$contractTerms$maturityDate
              
              factor$Data <- get.data.rate.reset(factor, anchor_dt, cycle, mat)
              riskFactor <- list(marketObjectCode = factor$label)
              
              if (is(factor, "YieldCurve")) {
                riskFactor$base <- 1
              } else {
                riskFactor$base <- factor$Data$Values[1]
              }
              riskFactor$data <- data.frame(time = rownames(factor$Data), 
                                           value =  as.character(factor$Data$Values))
              riskFactor$data$time <- paste0(riskFactor$data$time,"T00:00:00")
              riskFactor <- list(riskFactor)
            }else{
              riskFactor <- list()
            }
            
            fin_list <- list(contracts = contractDefs,
                             riskFactors = riskFactor)
            
            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = TRUE)
            
            # configure httr to NOT check certificTES on SSL / https connection
            httr::set_config(httr::config(ssl_verifypeer = 0L,
                                          ssl_verifyhost = 0L))
            
            # issue POST command to have server generate cashflows
            response_events <- POST(paste0(serverURL, "eventsBatch"),
                                    body = request_body,
                                    content_type_json())

            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(request_body)
              print(response_events)
              print(response_content)
              stop("ErrorIn::ContractType::API response error; Check if all necessary contractTerms were set correctly!!!")
            }
            
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- response_content[[1]]
            
            #first cashflow from single contract ptf
            stopifnot (cshfl_rslt1$status == "Success") # possible better info
            evs_list <- cshfl_rslt1$events
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- object$contractTerms$contractID
            evs$contractType <- object$contractTerms$contractType
            evs$statusDate <-  object$contractTerms$statusDate
            evs$riskFactors <- if(length(riskFactor) != 0) list(factor) else list()

            # construct the 7 columns with event list data (no long loops please)
            # initialize the data.frame with a row index evid
            evid <- 1:length(evs_list)
            events_df <-data.frame(evid)
            Event_Field_Names <- c("type","time","payoff","currency",
                                   "nominalValue","nominalRate","nominalAccrued")
            for(evfield in Event_Field_Names) {
              events_df[evfield] <- unlist(sapply(evs_list,
                                                  function(ev){ev[evfield]}))
            }
            # now remove the evid column used to size events_df
            events_df <- subset(events_df, select = -evid) #drop starter column

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            events_df$payoff <- round(events_df$payoff,2)
            events_df$nominalValue <- round(events_df$nominalValue,2)
            events_df$nominalAccrued <- round(events_df$nominalAccrued,2)
            evs$events_df <- events_df
            return(evs)
          })


# EventSeries methods for Operations contract

setMethod(f = "EventSeries", signature = c("FEMSContract", "character", "missing"),
          definition = function(object, processor, riskFactors){
            EventSeries(object,timeDate(substring(processor,1,10)))
          })

setMethod(f = "EventSeries", signature = c("FEMSContract", "AD0", "missing"),
          definition = function(object, processor, riskFactors){
            EventSeries(object,as.character(processor))
          })

setMethod(f = "EventSeries", signature = c("OperationalCF", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            
            # create event series object
            out <- new("EventSeries")
            out$contractID <- object$contractTerms$contractID
            out$contractType <- object$contractTerms$contractType
            
            # AD0 event
            events <- data.frame(time=as.character(processor),
                                 payoff=0.0,
                                 type="AD0",
                                 currency=object$contractTerms$currency,
                                 nominalValue=0.0,
                                 nominalRate=0.0,
                                 nominalAccrued=0.0)
            
            ops <- do.call(object$pattern, object$args)

            if(!is.null(ops)) {
              vals <- as.numeric(series(ops))
              events <- rbind(events, data.frame(time=as.character(time(ops)),
                                                 payoff=c(vals[1],vals[2:length(vals)]),
                                                 type="OPS", 
                                                 currency=object$contractTerms$currency,
                                                 nominalValue=0.0, 
                                                 nominalRate=0.0, 
                                                 nominalAccrued=0.0)
                              )
            }

            # convert to (sorted) timeSeries
            # Note: AD0 event needs to be after all other events of the same instant
            tms <- paste0(events$time,"T00:00:00")
            tms[events$type=="AD0"] <- paste0(substring(tms[events$type=="AD0"],1,10),"T23:59:59")
            events <- events[order(tms),]
            evs.ts <- timeSeries(events,timeDate(events$time))
            
            # compute nominal value
            evs.ts$nominalValue <- cumsum(evs.ts$nominalValue)
            
            # exclude pre-ad0 events
            # Note, its a sorted series so just look for AD0-event index
            evs.ts <- tail(evs.ts,nrow(evs.ts)-(which(evs.ts$type=="AD0")-1))
            
            # convert back to data.frame
            events <- as.data.frame(series(evs.ts))
            events$payoff <- as.numeric(events$payoff)
            events$nominalValue <- as.numeric(events$nominalValue)
            events$nominalRate <- as.numeric(events$nominalRate)
            events$nominalAccrued <- as.numeric(events$nominalAccrued)
            rownames(events) <- NULL
            
            # attach events to series
            out$events_df  <-  events
            
            return(out)
          })


setMethod(f = "EventSeries", signature = c("Investments", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            
            # create event series object
            out <- new("EventSeries")
            out$contractID <- object$contractTerms$contractID
            out$contractType <- object$contractTerms$contractType
            
            # AD0 event
            events <- data.frame(time=as.character(processor),
                                 payoff=0.0,
                                 type="AD0",
                                 currency=object$contractTerms$currency,
                                 nominalValue=0.0,
                                 nominalRate=0.0,
                                 nominalAccrued=0.0)
            
            ops <- do.call(object$pattern, object$args)

            if(!is.null(ops)) {
              if (length(ops)<2) stop("An investment pattern needs to have length>1!")
              vals <- c(ops[1,],diff(ops)[-1,])
              events <- rbind(events, data.frame(time=as.character(time(ops)),
                                                 payoff=c(-vals[1],vals[2:length(vals)]),
                                                 type=c("IED",rep("DPR",length(ops)-1)),
                                                 currency=object$contractTerms$currency,
                                                 nominalValue=vals,
                                                 nominalRate=0.0,
                                                 nominalAccrued=0.0
                                                 )
                              )
            }
            # If there is a salvage value (write-off no till 0)
            # we add a last event of type MD and the remaining value
            if ( tail(ops,1) > 0 ) {
              tmp <- tail(ops,1)
              vals <- as.numeric(series(tmp))
              events <- rbind(events, 
                              data.frame(time=as.character(time(tmp)),
                                         value=vals,
                                         payoff="MD",
                                         currency=object$contractTerms$currency,
                                         nominalValue=-vals,
                                         nominalRate=0.0,
                                         nominalAccrued=0.0
                                         )
                              )
              object$contractTerms$maturityDate <- events[events$type == "MD","time"]
            }else{
              min_idx <- min(which(events$nominalValue==0))
              object$contractTerms$maturityDate <- events[min_idx, "time"]
            }
            
            # convert to (sorted) timeSeries
            # Note: AD0 event needs to be after all other events of the same instant
            tms <- paste0(events$time,"T00:00:00")
            tms[events$type=="AD0"] <- paste0(substring(tms[events$type=="AD0"],1,10),"T23:59:59")
            events <- events[order(tms),]
            evs.ts <- timeSeries(events,timeDate(events$time))
            
            # compute nominal value
            evs.ts$nominalValue <- cumsum(evs.ts$nominalValue)
            
            # exclude pre-ad0 events
            # Note, its a sorted series so just look for AD0-event index
            evs.ts <- tail(evs.ts,nrow(evs.ts)-(which(evs.ts$type=="AD0")-1))
            
            # convert back to data.frame
            events <- as.data.frame(series(evs.ts))
            events$payoff <- as.numeric(events$payoff)
            events$nominalValue <- as.numeric(events$nominalValue)
            events$nominalRate <- as.numeric(events$nominalRate)
            events$nominalAccrued <- as.numeric(events$nominalAccrued)
            rownames(events) <- NULL
            
            # attach events to series
            out$events_df  <-  events
            
            return(out)
          })



## -----------------------------------------------------------------
## events methods for Operations contract
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "character", "missing"),
          definition = function(object, processor, riskFactors){
            return(FRSA:::events(object,timeDate(substring(processor,1,10))))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "AD0", "missing"),
          definition = function(object, processor, riskFactors){
            return(FRSA:::events(object, as.character(processor)))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            return(FRSA:::EventSeries(object,processor))
          })

