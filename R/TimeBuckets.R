#*************************************************************
# Copyright (c) 2018 by Wolfgang Breymann, ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' An S4 class that defines a time partition.
#' 
#' The partition is defined by a vector of class \code{timeDate} defining as 
#' sequence \eqn{t = t_1,\cdots,t_{n+1}}. 
#' The time intervals \eqn{\Delta_i} are left-open and right-closed, 
#' \eqn{\Delta_i = (t_1,t_{i+1}]}, \eqn{i = 1,\cdots,n}
#' 
#' @slot timeDate Defines the sequence \eqn{\{t_i\}}, \eqn{i = 1,\cdots,n}
#' @slot bucketLabs \code{character} vector containing labels for the time 
#'   intervals.
#' @slot breakLabs \code{character} vector containing labels for \eqn{\{t_i\}}.    
#' @export
#' @rdname tb-classes
setClass(
  "timeBuckets",
  slots = c(
    # t = "timeDate",
    bucketLabs = "character",
    breakLabs = "character")
  ,contains = "timeDate"
)

#' Constructor for class \code{timeBuckets}.
#' 
#' @param by Sequence of dates either as character strings of format 
#' \code{\%Y-\%m-\%d} or as \code{timeDate} object.
#' @param bucketLabs Chararacter vector containing the labels of the time 
#' buckets.
#' @param breakLabs Chracter vector containing the labels of the break points.
#' @return Object of class \code{timeBuckets}.
#' @export
#' @docType methods
#' @rdname tb-methods
# timeBuckets = function(t0, by, length.out, bucketLabs=NULL, breakLabs=NULL, ...) 
timeBuckets = function(by, bucketLabs=NULL, breakLabs=NULL, ...) 
{
  # tSeq <- timeSequence(substring(t0, 1, 10), by = by, length.out = length.out)
  if (class(by) == "character" ) 
    tSeq <- as.timeDate(substring(by, 1, 10))
  else 
    tSeq <- by
  
  if (length(tSeq)<2) stop("tSeq must be at least o length 2.")
  if (class(tSeq) == "character") {
    tSeq = timeDate(tSeq, ...)
  } else if (class(tSeq) == "timeDate") {
  } else {
    stop("t must be of class 'character' or 'timeDate'.")
  }
  if ( is.null(breakLabs) ) {
    breakLabs = as.character(tSeq) 
  }
  x = new("timeBuckets", tSeq, bucketLabs=as.character(bucketLabs), 
          breakLabs=as.character(breakLabs))
  x
}

#' \code{show} method for class \code{timeBuckets}.
#' 
#' @export
setMethod("show", signature=c("timeBuckets"),
          function(object)
          {
            x = object@Data
            labs = object@bucketLabs
            # finCenter = object@
            fmt = "%d.%m.%Y %H:%M:%S"
            len = length(x)
            cat("Time zone: ", object@FinCenter,"\n", sep="")
            for (i in 1:(len-1))
            {
              cat (
                labs[i],
                ": (",
                format (x[i], fmt), ",  ",
                format (x[i+1], fmt),"]\n", sep=""
              )
            }
            cat("breakLabs:", paste(object@breakLabs, collapse=", "))
          })

## @include
if (!isGeneric("print"))
#  #' @docType methods
#  #' @rdname print-methods
#' @export
  setGeneric(name = "print", 
             def = function(x, ...) {
               standardGeneric("print")
             })

#' \code{print} method for class \code{timeBuckets}.
#' 
#' @export
setMethod("print", signature=c("timeBuckets"),
          function(x) show(x)
          )

#' \code{print} method for class \code{timeBuckets}.
#' 
#' @return Labels for time buckets as defined in slot \code{bucketLabs}.
#' @export
setMethod("names", signature="timeBuckets",
          function(x) x@bucketLabs)

#' Returns the number of time intervals in a time partition.
#' 
#' @param x Object of class \code{timeBuckets}.
#' @return \code{integer} containing the number of time intervals in \code{x}.
#' @export
#' @docType methods
#' @rdname tb-methods
setMethod("length", signature="timeBuckets",
          function(x) {length(x@Data)-1})

## @include
#' Subscript method for class timeBuckets.
#'  
#' Since class timeBuckets provides a partition of a time interval, the value 
#' returned is the sequences of timeBuckets for the minimal enclosing set
#' of indices.
# #' @import timeDate
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("timeBuckets", "numeric", "missing"),
          definition = function(x, i, j, ...) {
            buLabs <- x@bucketLabs
            rr <- range(i)
            if (rr[1]<1 || rr[2]>length(x))
              stop("Subscript out of range")
            i <- rr[1]:(rr[2]+1)
            x@Data <- callGeneric(x@Data, i)
            x@breakLabs = as.character(x)
            x
          }
)


#' S4 class containing cash flows.
#' 
#' S4 class that contains cash flows occuring durring the time intervals defined
#' by an \code{timeBuckets} object.
#' 
#' This class extend \code{data.frame}.
#' 
#' @slot partition \code{timeBuckets} object.
#' @slot currency \code{character} with currency code.
#' @export
#' @docType methods
#' @rdname tb-methods
setClass("cfStream",
         slots = c(partition = "timeBuckets", currency="character"), 
         # prototype = prototype("currency"=NULL),
         contains = "data.frame"
         )

#' Constructor for class \code{cfStream}. 
#' 
#' @param buckets \code{timeBuckets} object containing the time intervals.
#' @param currency \code{character} with currency code.
#' @param data Cash flow data. Can be \code{numeric}, \code{integer}, or 
#'   \code{data.frame}.
#' @return \code{cfStream} object.
#' @export
#' @docType methods
#' @rdname tb-methods
cfStream = function(buckets, currency="None", data)
{
  # print(c(length(buckets),dim(data)[2]))
  if (class(data)=="numeric" || class(data)=="integer") 
    data = as.data.frame(matrix(data, nrow=1))
  rownames = rownames(data)
  # print(dim(data))
  if (length(buckets) != dim(data)[2]) 
    stop("No. of time buckets different from no. of data columns.")
  cf = new("cfStream", data, partition=buckets, currency=currency)
  cf@row.names = rownames
  return (cf)
}

#' Function that creates labels for \code{timeBuckets} object.
#' 
#' The labels have the form "from%sep%to". 
#' 
#' @param by \code{character} vector of dates.
#' @param format \code{character} containing the formatting information for 
#'   the "from" and "to" part of the output.
#' @param sep Separator of "from" and "to" part of output.
#' @return \code{character} vector with labels.
#' @export
#' @docType methods
#' @rdname tb-methods
periodNames = function(by, format="%d.%m.%y", sep="-")
{
  if (class(by)[1] != "character") by = as.character(by)
  tmp1 = as.Date(by[-length(by)])
  tmp1 = as.character(tmp1)
  tmp2 = as.Date(by[-1]) -1
  tmp2 = as.character(tmp2)
  
  nams = paste(format(as.Date(tmp1), format=format),
               format(as.Date(tmp2), format=format), sep=sep)
  nams
}


#' \code{print} method for class \code{cfStream}.
#' 
#' @export
setMethod("print", signature = c("cfStream"),
          function(x, format="%d.%m.%y", sep="-")
          {
            tB = x@partition
            t = tB@Data
            data = x@.Data
            if (length(tB@bucketLabs)>0 )
              x@names = tB@bucketLabs
            else
              x@names = periodNames(t)
            cat (paste("Currency:", x@currency, "\n", sep=" "))
            callNextMethod(x)
          })

#' \code{show} method for class \code{cfStream}.
#' 
#' This method is defined by means of the print method because otherwise 
#' the slots "partition" and "currency" are displayed in an ugly way 
#' through the default method of "show" for "data.frame".
#' @export
setMethod("show", signature = c("cfStream"),
          function(object)
          {
            print(object)            
          })

#' Converts dates with ACTUS \code{T00}/\code{T24} convention to standard dates,
#' \code{T00} being replaced by \code{00:00:01} and \code{T24} by \code{23:59:59}.
#' 
#' @param char Character string in ACTUS time format.
#' @return \code{timeDate} object.
#' @export
#' @docType methods
#' @rdname tb-methods
aDates2tDates = function(char)
{
  tshift = c(1, 3600*24-1)
  names(tshift) = c("00","24")

  tmp = as.data.frame(strsplit(char,"T"))
  colnames(tmp) = NULL
  idx = as.character(tmp[2,])
  as.timeDate(tmp[1,])+tshift[idx]
}

