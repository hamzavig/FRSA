#*******************************************************************************
# ZHAW
# package: PA_FEMS
# Date: 08.11.2022
# Autor: Vigan Hamzai (hamzavig@students.zhaw.ch)
#*************************************************************

#' @import data.tree
setOldClass("Node")

##############################################################
#' 
#' Class that contains the whole model of an enterprise or institution.
#' 
#' The class inherits from class \code{Node} in package \code{data.tree}
#' It contains a hierarchical model structure.
#' 
#' @import data.tree R6
#' @export
#' @rdname Institution
Institution <- R6Class("Institution",
                       inherit = Node)

# ************************************************************
# createInstitution(institutionName)
# ************************************************************
#' createInstitution
#' 
#' createInstitution(name) function of class Institution 
#' creates the whole hierarchy of the institution with name <name>.
#' 
#' @export
#' @rdname createInstitution
#' 
createInstitution <- function(name, ...) {
  
  institution <- Node$new(name)
  # Create top level nodes "Assets", "Liabilities" and "Operations"
  institution$AddChild("Assets")
  institution$AddChild("Liabilities")
  institution$AddChild("Operations")
  
  # Create underlying nodes for "Assets"
  institution$Assets$AddChild("ShortTerm")
  institution$Assets$AddChild("LongTerm")
  institution$Assets$AddChild("FixedAssets")
  
  institution$Assets$ShortTerm$AddChild("LiquidAssets")
  institution$Assets$LongTerm$AddChild("Loans")
  institution$Assets$LongTerm$AddChild("Mortgages")
  
  # Create underlying nodes for "Liabilities"
  institution$Liabilities$AddChild("ShortTerm")
  institution$Liabilities$AddChild("LongTerm")
  institution$Liabilities$AddChild("Equity")
  
  institution$Liabilities$ShortTerm$AddChild("Deposits")
  institution$Liabilities$LongTerm$AddChild("Loans")
  
  # Create underlying nodes for "Operations"
  institution$Operations$AddChild("Revenues")
  institution$Operations$AddChild("Expenses")
  
  institution$Operations$Revenues$AddChild("Commissions")
  institution$Operations$Revenues$AddChild("Rent")
  institution$Operations$Revenues$AddChild("Other")
  
  institution$Operations$Expenses$AddChild("Salaries")
  institution$Operations$Expenses$AddChild("Rent")
  institution$Operations$Expenses$AddChild("Other")
  
  return(institution)
}

# ************************************************************
# assignContracts2Tree(institution, ptf)
# ************************************************************
#' assignContracts2Tree
#' 
#' assignContracts2Tree(institution, ptf) assigns a given Portfolio <ptf>
#' to the respective leaf of the institution tree while converting the contracts
#' of the given Portfolio <ptf> into a data.frame first.
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @export
#' @rdname assignContracts2Tree

assignContracts2Tree <- function(institution, ptf, ...) {
  
  tree_dict <-   c("ASL" = institution$Assets$ShortTerm$LiquidAssets,
                   "ALL" = institution$Assets$LongTerm$Loans,
                   "ALM" = institution$Assets$LongTerm$Mortgages,
                   "AFA" = institution$Assets$FixedAssets,
                   "LSD" = institution$Liabilities$ShortTerm$Deposits,
                   "LLL" = institution$Liabilities$LongTerm$Loans,
                   "LEQ" = institution$Liabilities$Equity,
                   "ORC" = institution$Operations$Revenues$Commissions,
                   "ORR" = institution$Operations$Revenues$Rent,
                   "ORO" = institution$Operations$Revenues$Other,
                   "OES" = institution$Operations$Expenses$Salaries,
                   "OER" = institution$Operations$Expenses$Rent,
                   "OEO" = institution$Operations$Expenses$Other
                   )

  for(i in 1:length(ptf$contracts)){
    
    contractID <- getCIDfromContract(ptf$contracts[[i]])
    ct_leaf_key <- substr(contractID,1,3)
    leaf <- tree_dict[ct_leaf_key]
    
    stopifnot(leaf[[ct_leaf_key]]$isLeaf)
    leaf[[ct_leaf_key]]$contracts <- c(leaf[[ct_leaf_key]]$contracts, ptf$contracts[[i]])
  }

  return(institution)
}


# ************************************************************
# addMarketObject2Contracts(institution, yc, cylce)
# ************************************************************
#' addMarketObject2Contracts
#' 
#' addMarketObject2Contracts(institution, yc, cylce) assigns a given
#' MarketObject (YieldCurve) to the contracts which it applies to
#' 
#' @include ContractType.R
#' @include YieldCurve.R
#' 
#' @export
#' @rdname addMarketObject2Contracts

addMarketObject2Contracts <- function(institution, yc, spread, cycle, ...) {
  
  for(i in 1:length(institution$leaves)){
    
    leaf <- institution$leaves[[i]]
    
    if (!is.null(leaf$contracts)){
    
    for(j in 1:length(leaf$contracts)){
      
      if(leaf$contracts[[j]]$contractTerms$contractType == "ANN"){
        
        leaf$contracts[[j]]$contractTerms$marketObjectCodeOfRateReset <- yc$label
        leaf$contracts[[j]]$contractTerms$rateSpread <- spread
        leaf$contracts[[j]]$contractTerms$cycleOfRateReset <- cycle
        
        if(cycle == "P1YL1"){
          anchorDate <- as.character(ymd(leaf$contracts[[j]]$contractTerms$initialExchangeDate) %m+% years(1))
          leaf$contracts[[j]]$contractTerms$cycleAnchorDateOfRateReset <- anchorDate
        }else if(cycle == "P6ML1"){
          anchorDate <- as.character(ymd(leaf$contracts[[j]]$contractTerms$initialExchangeDate) %m+% months(6))
          leaf$contracts[[j]]$contractTerms$cycleAnchorDateOfRateReset <- anchorDate
        }else if(cycle == "P1ML1"){
          anchorDate <- as.character(ymd(leaf$contracts[[j]]$contractTerms$initialExchangeDate) %m+% months(1))
          leaf$contracts[[j]]$contractTerms$cycleAnchorDateOfRateReset <- anchorDate
        }else{
          stop("Cycle not known")
        }
      }
    }
    }
  }
  
  return(institution)
}


#' @include Events.R
#' @include EventSeries.R
#' @include RiskFactorConnector.R
#' @rdname events-methods
#' @export
setMethod(f = "events", signature = c("Node", "missing", "RiskFactorConnector"),
          definition = function(object, processor, riskFactors) {
            clearEvents(object)
            object$Do(fun=addEvents, rf = riskFactors, filterFun=isLeaf)
            
            return(object)
          })

# ************************************************************
# addEvents(node)
# ************************************************************
#' addEvents
#' 
#' addEvents(node) assigns all corresponding events of the contracts
#' in the respective leaf of the institution tree
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @include EventSeries.R
#' @export
#' @rdname addEvents

addEvents <- function(node, ...){
  
  node$events <- NULL
  pars = list(...)
  ctrs = node$contracts
  
  res = sapply(X=ctrs,
               FUN = function(x, pars) {
                 
                 if(x$contractTerms$contractType %in% c("PAM","ANN")){

                   serverURL <- "https://demo.actusfrf.org:8080/"
                   riskFactors <- pars[[1]]
                   
                   ctr_events <- EventSeries(x, serverURL, riskFactors)
                 }else{
                   
                   ctr_start <- x$contractTerms$initialExchangeDate
                   riskFactors <- pars[[1]]
                   
                   ctr_events <- EventSeries(x, ctr_start)
                 }
                 
                 if (!is.null(ctr_events) ) {
                   if (is.null(node$events)) {
                     node$events <- list()
                   }
                   node$events <- c(node$events, ctr_events)
                 }
                 
              }, pars)
}



# ************************************************************
# getEvents(node, cid)
# ************************************************************
#' getEvents
#' 
#' getEvents(node, cid) gets all corresponding events of the contract
#' in the respective leaf of the institution tree
#' 
#' @include EventSeries.R
#' @export
#' @rdname getEvents

getEvents <- function(node, cid, ...){
  
  for (i in 1:length(node$leaves)){
    leaf_evs <- node$leaves[[i]]$events
    
    for(j in 1:length(leaf_evs)){
      
      if(leaf_evs[[j]]$contractID == cid){
        return(leaf_evs[[j]])
      }
    }
  }
}



# ************************************************************
# getLeafsAsDataFrames(institution)
# ************************************************************
#' getLeafsAsDataFrames
#' 
#' getLeafsAsDataFrames(institution) converts each leaf and it's contracts
#' to a data.frame and returns a list of data.frames.
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @export
#' @rdname getLeafsAsDataFrames

getLeafsAsDataFrames <- function(institution, ...) {
  
  leaf_paths <- c("Assets$ShortTerm$LiquidAssets",
                  "Assets$LongTerm$Loans",
                  "Assets$LongTerm$Mortgages",
                  "Assets$FixedAssets",
                  "Liabilities$ShortTerm$Deposits",
                  "Liabilities$LongTerm$Loans",
                  "Liabilities$Equity",
                  "Operations$Revenues$Commissions",
                  "Operations$Revenues$Rent",
                  "Operations$Revenues$Other",
                  "Operations$Expenses$Salaries",
                  "Operations$Expenses$Rent",
                  "Operations$Expenses$Other"
  )
  
  leaf_dfs <- list()
  
  for(i in 1:length(institution$leaves)){
    
    if(!is.null(institution$leaves[[i]]$contracts)){
      leaf_ptf <- Portfolio()
      leaf_ptf$contracts <- institution$leaves[[i]]$contracts
      leaf_df <- getPortfolioAsDataFrame(leaf_ptf)
    }else{
      leaf_df <- data.frame()
    }
    
    leaf_dfs[[i]] <- list(leaf = leaf_paths[i],
                          contracts = leaf_df)
  }
  
  return(leaf_dfs)
}

####--------------------------------------------------------------------------
## value methods

#' @include Value.R
#' @include TimeBuckets.R
#' @rdname val-methods
#' @export
setMethod(f = "value", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, method, scale=1, digits=2) {
            
            if (missing(method)) {
              method <- DcEngine()
            }
            if (missing(type)) {
              type <- "nominal"
            }
            res <- value(object, as.timeDate(by), type=type, method=method,
                         scale=scale, digits=digits)
            colnames(res) <- by@breakLabs
            return(res)
          })


#' @include Value.R
#' @rdname val-methods
#' @export
setMethod(f = "value", signature = c("Node", "timeDate", "ANY"),
          definition = function(object, by, type, method, scale=1, digits=2) {
            if (missing(method)) {
              method <- DcEngine()
            }
            if (missing(type)) {
              type <- "nominal"
            }
            # Compute value for whole tree
            clearAnalytics(object, "value")
            
            object$Do(fun=fAnalytics, "value", by=as.character(by), type=type,
                      method=method, filterFun=isLeaf)
            
            aggregateAnalytics(object, "value")
            
            object$Liabilities$Equity$value <- -object$value
            object$Liabilities$value <- object$Liabilities$ShortTerm$value + object$Liabilities$LongTerm$value + object$Liabilities$Equity$value
            object$value <- rep(0, length(object$value))
            
            object2 <- Clone(object)
            if ( type == "nominal" && is.element("Operations", names(object2$children)) )
              object2$RemoveChild("Operations")
            
            res <- data.frame(
              t(object2$Get("value", format = function(x) as.numeric(ff(x,0)))  ),
              check.names=FALSE, fix.empty.names=FALSE)
            # res <- value(object, as.character(by), type=type, method=method,
            #              scale=scale, digits=digits)
            rownames(res) <- capture.output(print(object2))[-1]
            colnames(res) <- as.character(by)
            return(round(res/scale,digits))
          })


####--------------------------------------------------------------------------
## income methods

#' @include Income.R
#' @include TimeBuckets.R
#' @rdname inc-methods
#' @export
setMethod(f = "income", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, revaluation.gains, 
                                method, scale=1, digits=2){
            
            # Compute income for whole tree
            if (missing(method)) {
              method <- DcEngine()
            }
            if (missing(type)) {
              type <- "marginal"
            }
            if (missing(revaluation.gains)) {
              revaluation.gains <- FALSE
            }
            clearAnalytics(object, "income")
            
            object$Do(fun=fAnalytics, "income", by=by, type=type, method=method, 
                      revaluation.gains=revaluation.gains, filterFun=isLeaf)
            
            aggregateAnalytics(object, "income")
            
            res <- data.frame(
              t(object$Get("income", format = function(x) as.numeric(ff(x,0))) ),
              check.names=FALSE, fix.empty.names=FALSE)
            
            rownames(res) <- capture.output(print(object))[-1]
            colnames(res) <- by@bucketLabs
            return(round(res/scale,digits))
          })



####----------------------------------------------------------------------------
## liquidity

#' @include Liquidity.R
#' @rdname liq-methods
#' @export
setMethod(f = "liquidity", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, scale=1, digits=2){
            if (missing(type)) {
              type <- "marginal"
            }
            # Compute liquidity for whole tree
            clearAnalytics(object, "liquidity")
            object$Do(fun=fAnalytics, "liquidity", by=by, type=type, 
                      filterFun=isLeaf)
            aggregateAnalytics(object, "liquidity")
            res = data.frame(
              t(object$Get("liquidity", format = function(x) as.numeric(ff(x,0))) ),
              check.names=FALSE, fix.empty.names=FALSE)
            rownames(res) = capture.output(print(object))[-1]
            colnames(res) <- by@bucketLabs
            return(round(res/scale,digits))
          })


####----------------------------------------------------------------------------
## default

#' @include Default.R
#' @rdname def-methods
#' @export
#' 
setMethod(f = "default", signature = c("Node", "list", "character", "list"),
          definition = function(object, defaults, from, rawCtrs){
            
            leafs <- object$Assets$leaves
            leafs[length(leafs)] <- NULL
            defCtrs <- list()
            
            for(leaf in leafs){
              ctrs <- determineDefault(leaf, defaults, from, rawCtrs)
              
              for(j in 1:length(ctrs)){
                defCtrs <- append(defCtrs, ctrs[[j]])
              }
            }
            
            object$Assets$AddChild("Default")
            object$Assets$Default$contracts <- defCtrs
            
          })


#' @include Default.R
#' @rdname def-methods
#' @export
#' 
determineDefault <- function(node, defaults, from, rawCtrs){
  
  ctrList <- list()
  
  if(!is.null(node$contracts) || lenght(node$contracts) > 0){
    
    ctrs <- node$contracts
    defaultLabels <- c()
    for(i in 1:length(defaults)) defaultLabels <- c(defaultLabels, defaults[[i]]$label)
    
    for(ctr in ctrs){
      if(ctr$contractTerms$legalEntityIDCounterparty %in% defaultLabels &&
         ctr$contractTerms$maturityDate > from){
        
        cid <- ctr$contractTerms$contractID
        
        for(i in 1:length(rawCtrs)){
          ctrdf <- rawCtrs[[i]]
          
          if(cid %in% ctrdf[,"contractID"]){
            rawCtr <- ctrdf[ctrdf$contractID == cid,]
            break
          }else{
            next
          }
        }
        
        defCtrs <- generateDefaultContracts(ctr, defaults, from, rawCtr)
        
        for(j in 1:length(defCtrs)){
          
          ctrList <- append(ctrList, defCtrs[[j]])
        }
      }
    }
  }
  return(ctrList)
}



####----------------------------------------------------------------------------
## sensitivity

#' @include Sensitivity.R
#' @rdname sen-methods
#' @export
#' 
setMethod(f = "sensitivity", signature = c("Node", "YieldCurve"),
          definition = function(object, yield){
            # Compute sensitivity for whole tree
            clearAnalytics(object, "sensitivity")
            object$Do(fun=fSensitivityLeaf, "sensitivity", yield = yield, filterFun=isLeaf)
            object$Do(fun=fSensitivityAggregation, "sensitivity", yield = yield)
          })


####----------------------------------------------------------------------------
## sensitivity

#' @include Sensitivity.R
#' @rdname sen-methods
#' @export
#' 
setGeneric(name = "showSensitivity", def = function(node, ...){
  standardGeneric("showSensitivity")
})


#' @include Sensitivity.R
#' @rdname sen-methods
#' @export
#' 
setMethod(f = "showSensitivity", signature = c("Node"),
          definition = function(node){
            
            nodes <- Traverse(
              node,
              traversal = c("pre-order")
            )
            
            df <- data.frame()
            
            for(i in 1:length(nodes)){
              df <- rbind(df, nodes[[i]]$sensitivity)
            }
            
            nodes.path <- Get(Traverse(node),"pathString")
            rn <- capture.output(node)[-1]
            rn <- substring(rn,4,max(nchar(rn)))
            res <- df
            rnams <- character(nrow(res))
            
            for (i in 1:nrow(res)) {
              rnams[i] <- paste(format(i,width=2),rn[nodes.path==res[i,1]])
            }
            
            rownames(res) <- rnams
            return(res[,-1])
          })



##################################################################################
#' specific function for computing sensitivity analytics on a data.tree structure 
#' of class Node
#'
#' This function computes analytics individually for the leafs of a tree
#' The analytics to be computed must be passed as first argument.
#' This function thus subsumes the function of all three specialized 
#' functions above (which are commented out)
 
fSensitivityLeaf = function(node, ...) {
  
  pars = list(...)
  # clear analytics
  node[[pars[[1]]]] <- NULL
    
  if(is.null(node$events) || length(node$events)==0){
    res <- data.frame(nodePath = node$pathString,
                      ID = node$name,
                      PresentValue = 0,
                      Duration = 0)
  }else{
    ctrs = node$contracts
    resPV = sapply(X=1:length(ctrs),
                   FUN = function(x, pars) {
                     pars = list(...)
                     fnam = "presentValue"
                     Id = x
                     object = node$events[[Id]]
                     pars = pars[c(-1)]
                     do.call(fnam, c(object=object, pars))
                   })
    resD = sapply(X=1:length(ctrs),
                  FUN = function(x, pars) {
                    pars = list(...)
                    fnam = "duration"
                    Id = x
                    object = node$events[[Id]]
                    pars = pars[c(-1)]
                    do.call(fnam, c(object=object, pars))
                  })
    resCtrs = sapply(X=ctrs,
                     FUN = function(x){
                       as.character(x$contractTerms$contractID)
                     })
    res <- data.frame(nodePath = rep(node$pathString,length(resCtrs)),
                      ID = resCtrs,
                      PresentValue = resPV,
                      Duration = resD)
  }
  
  node[[pars[[1]]]] <- res
}


##################################################################################
#' specific function for computing sensitivity analytics on a data.tree structure 
#' of class Node
#'
#' This function computes analytics individually for the leafs of a tree
#' The analytics to be computed must be passed as first argument.
#' This function thus subsumes the function of all three specialized 
#' functions above (which are commented out)

fSensitivityAggregation = function(node, ...) {
  
  if(!node$isLeaf){
    
    pars = list(...)
    # clear analytics
    node[[pars[[1]]]] <- NULL
    
    children <- node$children
    
    resPV <- sapply(X=1:length(children),
                    FUN = function(i){
                      leaves <- children[[i]]$leaves
                      leafPVs <- sapply(X=1:length(leaves),
                                        FUN = function(j){
                                          leaves[[j]]$sensitivity$PresentValue
                                        })
                      leafPVs
                    })
    resPV.vector <- unlist(resPV)
    resPV.vector
    
    resD <- sapply(X=1:length(children),
                   FUN = function(i){
                     leaves <- children[[i]]$leaves
                     leafDs <- sapply(X=1:length(leaves),
                                      FUN = function(j){
                                        leaves[[j]]$sensitivity$Duration
                                      })
                     leafDs
                   })
    resD.vector <- unlist(resD)
    resD.vector
    
    pv = sum(resPV.vector)
    d = round(as.numeric(t(resPV.vector/sum(resPV.vector))%*%resD.vector),2)
    
    res <- data.frame(nodePath = node$pathString,
                      ID = "-",
                      PresentValue = pv,
                      Duration = d)
    
    node[[pars[[1]]]] <- res
  }
}


##################################################################################
#' general function for computing analytics on a data.tree structure of class Node
#'
#' This function computes analytics individually for the leafs of a tree
#' The analytics to be computed must be passed as first argument.
#' This function thus subsumes the function of all three specialized 
#' functions above (which are commented out)
fAnalytics = function(node, ...) {
  
  pars = list(...)
  # clear analytics
  node[[ pars[[1]] ]] <- NULL
  if ( is.null(node$events) || length(node$events)==0 ) {
    node[[ pars[[1]] ]] <- rep(0, length(pars[["by"]]))
    if ( is.null(names(pars[["by"]])) ) {
      names(node[[pars[[1]] ]]) = as.character(pars[["by"]])
    } else {
      names(node[[pars[[1]] ]]) = names(pars[["by"]])
    }
  } else {
    ctrs = node$contracts
    res = sapply(
      X=1:length(ctrs),
      FUN = function(x, pars) {
        pars = list(...)
        fnam = pars[[1]] # the name of the analytics [liquidity|income|value]
        Id = x
        object = node$events[[Id]] # the eventSeries of the contract
        pars = pars[c(-1)]
        do.call(fnam, c(object=object, pars))
      })
    if (!is.null(dim(res)) ) {
      res = rowSums(res)
    } else if (length(res) == 0) {
      res <- NULL
    }
    node[[pars[[1]] ]] = res
  }
}

# This function aggregates the results computed by fAnalytics
aggregateAnalytics = function(node, analytics) {
  if (!isLeaf(node)) {
    res = sapply(
      node$children,
      FUN=function(child, analytics) {
        x = analytics
        if (!is.null(child[[x]])) {
          child[[x]]
        } else if (!isLeaf(child)) {
          aggregateAnalytics(child, analytics=x)
        }
      }, analytics=analytics, simplify=TRUE)
    if ( !is.null(dim(res)) ) res = rowSums(res)
    node[[analytics]] = res
  }
}

# Clears previously computed the analytics "analytics" from the tree "node"
clearAnalytics = function(node, analytics) {
  node[[analytics]] = NULL
  nodes = Traverse(node, traversal="pre-order")
  for (n in nodes) {
    n[[analytics]] = NULL
  }
}

#' Clears previously computed the analytics "analytics" from the tree "node"
#' @export
clearEvents = function(node) {
  clearAnalytics(node, "events")
}


# Formatting function.
# Notice that the 'ifelse' command doesn't return the right result.
ff = function (x, digits = 3) 
{
  if (is.null(x) || is.na(x) ) {
    ch = ""
  } else {
    ch = sprintf(paste0("%.", digits, "f"), x)
    names(ch) = names(x)
  }
  return(ch)
}
