library(utils)
library(timeSeries)

# ***************************************
# file2dataframe(filename)
#    reads named file; creates clean df
#    unify file2Contracts_df(filename)
#    and file2riskFactors_df(filename)
#       FNP 25th April 2022
#    for contracts we need to force dayCountConvention to be read
#    as character ( "30E360" would be read numeric )  BUT
#    field does not occur in riskData csv files
# ***************************************

#' @export
#' 
contractFile2dataframe <- function(cdfn, sep = ",") {
  # read csv ignores and strips '"' and dayCountConvention has 30E360 value
  # which gets read as numeric = Inf.
  # we assume all contract csv files will have a dayCountConvention column
  df <- utils::read.csv(cdfn, colClasses = c(dayCountConvention = "character"))
  # convert all missing data into text null
  df[is.na(df)] <- "NULL"
  return(df)
}

operationFile2dataframe <- function(odfn, sep = ",") {
  df <- utils::read.csv(odfn)
  df[is.na(df)] <- "NULL"
  return(df)
}

riskFile2dataframe <- function(fname, sep = ","){
  # this read.csv works for csv with no dayCountConvention column. Warning
  df = utils::read.csv(fname)
  # convert all missing data into text null
  df[is.na(df)] <- "NULL"
  return(df)
}

# ************************************
# contracts_df2list(contracts_df)
#   build list of contracts from df
#   expanded cleaned version of df2contracts_list
#  Split df: terms, legs, descriptp (once)
#  for each row: createContract(terms, legs, irow)
#      append into returned list  fnp  10 Apr 2022
#    -- improved version df2contracts_list()
# ************************************************

contracts_df2list<- function(contracts_df){
  nonTermCols <- c("description","contrStrucObj.marketObjectCode",
                   "contrStruc.referenceType", "contrStruc.referenceRole")
  terms_df <-contracts_df[!names(contracts_df) %in% nonTermCols]
  legs_df <-data.frame(
    marketObjectCode = contracts_df["contrStrucObj.marketObjectCode"],
    referenceType = contracts_df["contrStruc.referenceType"],
    referenceRole = contracts_df["contrStruc.referenceRole"]
  )
  outlist <- list()
  for ( irow in 1:nrow(contracts_df)){
    outlist <- append (outlist, datarow2Contract(terms_df,legs_df,irow) )
  }
  return (outlist)
}


# ************************************************************
# operations_df2list(operations_df)
# build list of operations from df
#  Split df: terms, pattern, args (once)
#  for each row: createOperations(terms, pattern, args, irow)
# ************************************************************
operations_df2list <- function(operations_df) {
  nonTermCols <- c("repetition", "frequency",	"times", "inverted", "description")
  terms_df <- operations_df[!names(operations_df) %in% nonTermCols]
  
  outlist <- list()
  for(irow in 1:nrow(operations_df)){
    
    timeSeq <- timeSequence(from = timeDate(operations_df$initialExchangeDate[irow]),
                            by = operations_df$frequency[irow],
                            length = operations_df$times[irow])
    
    if(operations_df$contractType[irow] == "Investments"){
      
      pattern <- function(value, n, times){
        timeSeries(seq(value, 0, length.out=n), times)
      }
      
      if (operations_df$contractRole[irow] == "long"){
        notionalPrincipal <- operations_df$notionalPrincipal[irow]
      }else{
        notionalPrincipal <- -operations_df$notionalPrincipal[irow]
      }
      
      args <- list(value = notionalPrincipal,
                   n = operations_df$times[irow],
                   times = timeSeq)
    }else if(operations_df$contractType[irow] == "OperationalCF") {
      
      pattern <- function(dat, times){
        timeSeries(data=dat, charvec=times)
      }
      
      if (operations_df$contractRole[irow] == "long"){
        notionalPrincipal <- operations_df$notionalPrincipal[irow]
      }else{
        notionalPrincipal <- -operations_df$notionalPrincipal[irow]
      }
      
      cfs <- rep(notionalPrincipal, operations_df$repetition[irow])
      
      timesIdx <- if (operations_df$inverted[irow]) -operations_df$times[irow] else -1
      args <- list(dat = cfs,
                   times = timeSeq[timesIdx])
      
    }else{
      stop("Operations: No known contract type.")
    }
    
    outlist <- append(outlist, datarow2Operation(terms_df, pattern, args, irow))
  }
  return(outlist)
}


# ************************************
# riskFactors_df2list(riskFactors_df)
#   input: dataframe riskFactor data,
#   returns list of riskFactor objects
#   convert date, value pairs in risk Factor row
#   all riskFactors are YieldCurves for now
# ************************************************

riskFactors_df2list <- function(riskFactors_df){
  
  rfList <- list()
  
  for(irow in 1:nrow(riskFactors_df)){
    
    if(riskFactors_df[irow,"rfType"] == "YieldCurve"){
      label <- riskFactors_df[irow, "label"]
      referenceDate <- riskFactors_df[irow, "referenceDate"]
      tenors <- as.character(riskFactors_df[irow, grepl("tenor", names(riskFactors_df))])
      rates <- as.numeric(riskFactors_df[irow, grepl("rate", names(riskFactors_df))])
      
      yc <- YieldCurve(label = label, 
                       ReferenceDate = referenceDate, 
                       Tenors = tenors,
                       Rates = rates)
      rfList <- append(rfList, yc)
      
    }else if(riskFactors_df[irow,"rfType"] == "DefaultCurve"){
      
      label <- riskFactors_df[irow, "label"]
      referenceDate <- riskFactors_df[irow, "referenceDate"]
      tenors <- as.character(riskFactors_df[irow, grepl("tenor", names(riskFactors_df))])
      rates <- as.numeric(riskFactors_df[irow, grepl("rate", names(riskFactors_df))])
      
      dc <- DefaultCurve(label = label, 
                       ReferenceDate = referenceDate, 
                       Tenors = tenors,
                       Rates = rates)
      rfList <- append(rfList, dc)  
      
    }else{
      stop("Other risk factors than Yield Curves are not supported yet!")
    }
  }
  return(rfList)
}

# ***********************************************
# datarow2Contract ( ) -  create contract object
#    inputs:  terms_df, legs_df, descr, irow :
#    contractType; object <- new("contractType)
#    constructors will set isStructured but not populate terms or legs
#    if isStructured: insertLegs
#    insertTerms ( both simple and structured cases )
# ************************************************
datarow2Contract<- function(terms_df, legs_df,irow){
  contractTypeName <- longName(tolower(terms_df$contractType[irow]))
  contract <- CT(contractTypeName)
  #FNP  avoid validity check for now 10Apr2022; test PAM,OPTNS
  if (contractTypeName == "Option"){
    contract$contractStructure<-list(
      CLeg(legs_df$contrStrucObj.marketObjectCode[irow])
    )
    contract$isCompound <- TRUE
  } else {
    contract$isCompound <- FALSE
  }
  # insert terms - skipping term validy checks for now FNP Apr 2022
  contractTerms <- as.list(t(terms_df[irow,]))
  names(contractTerms) <- colnames(terms_df)  # reattach column names
  
  ## drop all NULL elements. sapply operates on a list but returns a vector
  contractTerms <- contractTerms[sapply(contractTerms, function(x) x != "NULL")]
  
  set(object = contract, what = contractTerms)
  return(contract)
}


datarow2Operation <- function(terms_df, pattern, args, irow){
  contractTypeName <- terms_df$contractType[irow]
  operation <- FEMSCT(contractTypeName)
  operation$pattern <- pattern
  operation$args <- args
  
  contractTerms <- as.list(t(terms_df[irow,]))
  names(contractTerms) <- colnames(terms_df)

  set(object = operation, what = contractTerms)
  
  return(operation)  
}

# ***********************************************
# longName(name)
#    get long name for ContractType from short
# ************************************************

longName <- function(name) {
  short <- c("pam", "ann", "nam", "lam", "stk",
             "fxout", "swaps", "futur", "optns","lax")
  long <- c("principalatmaturity", "annuity", "negativeamortizer",
            "linearamortizer", "stock", "foreignexchangeoutright",
            "swap", "future", "option","exoticlinearamortizer")
  target <- c("PrincipalAtMaturity", "Annuity", "NegativeAmortizer",
              "LinearAmortizer", "Stock", "ForeignExchangeOutright",
              "Swap", "Future", "Option","ExoticLinearAmortizer")
  names.table <- data.frame(short=short, long=long, target=target)
  if(tolower(name)%in%short) {
    out <- names.table[which(short==tolower(name)), "target"]
  } else if(tolower(name)%in%long) {
    out <- names.table[which(long==tolower(name)), "target"]
  } else {
    stop(paste("ContractType", name, "does not exist!", sep=" "))
  }
  return(out)
}



# -----------------------------------------------------------------
# private util methods
# get rates from YieldCurve for rate reset schedule 
get.data.rate.reset <-  function(yc, anchor_dt, cycle, end_dt, ISO = TRUE){

  times <- as.character(timeSequence(
    from = anchor_dt,  
    to = timeSequence(end_dt, by = convert.Duration(cycle, ISO), length.out = 2)[2],
    by = convert.Duration(cycle, ISO)))
  
  if (class(yc) == "YieldCurve" || class(yc) == "DynamicYieldCurve") {
    data <- getRateAt(yc, times[2:length(times)], times[1:length(times)-1])
  } else {
    data <- rep(yc, length(times)-1)
  }
  df <- timeSeries(data = data,
                   charvec = times[1:length(times)-1],
                   units = "Values")
  return(df)
}

convert.cycle <- function(cycle) {
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  return(by)
}

convert.Duration <- function(duration, ISO) {
  
  if (ISO){
    # currently, just take the 2nd and 3rd element
    n_units <- substr(duration, 2, 2)
    units <- substr(duration, 3, 3)
    
    conv_units <- switch(units, "Y" = "years", 
                         "Q" = "quarter", 
                         "M" = "months", 
                         "D" = "days")
    out <- paste(n_units,conv_units)
  } else {
    out <- convert.cycle(duration)
  }
  return(out)
}
