#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class to represent an Analysis Date
#' 
#' Actus Contract Types (CT) generate post-\code{AD0} cash flows. At the same time,
#' \code{AD0} marks the point in time as per which (market) risk factors have to be
#' know in order for a linked CT to be able to derive cash flows.
#' Hence, both, java CTs and java risk factor objects need to know AD0 in java
#' ZonedDateTime format. An \code{AD0} object provides this reference.
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{generateEvents, processEvents, granularResults}}
#'
## @examples
#' 
## @include
#' @export 
## @docType
#' @rdname ad0-classes
setRefClass("AD0",
            fields = list(
            timestamp = "character"
            ))

##############################################################
#' AD0-class constructor
#'
#' Create an instance of \code{AD0} class passing the date in character format. The
#' format of the character date in order for it to be converted to a Java
#' ZonedDateTime date has to be \code{YYYY-MM-DDTXX} where XX is either '00' or '24' 
#' indicating the timestamp (beginning of day/end of day).
#'
#' @param date The character date of format \code{YYYY-MM-DDTXX} that will be
#' used as Analysis Date
#' 
#' @param ...
#'
#' @return An object of class AD0 containing the reference to the Java date
#' 
#' @seealso \code{\link{processEvents}} and \code{\link{processEvents}}
#'
## @include
#' @export
#' @docType methods
#' @rdname ad0-methods
## @aliases
setGeneric(name = "AD0",
           def = function(date, ...){
             standardGeneric("AD0")
           })

## @include
#' @export
#' @rdname ad0-methods
#' @aliases AD0,character-method
setMethod(f = "AD0",signature = c("timeDate"),
          definition = function(date, ...){
            AD0(as.character(date))
          })

## @include
#' @export
#' @rdname ad0-methods
#' @aliases AD0,character-method
setMethod(f = "AD0",signature = c("character"),
          definition = function(date, ...){
              ad0 <- new("AD0")
              if(length(grep("T",date))==0) date=paste0(date,"T00")
              ad0$timestamp <- date
              return(ad0)
          })

##############################################################
#' Type-cast an AD0-object to character
#'
#' Type-cast an AD0-object, i.e. the Java reference, back to an R-character
#' object of format \code{YYYY-MM-DDTXX}.
#'
#' @param object An object of class AD0 containing the Java reference to be
#' casted
#'
#' @return An R-character representing the Java date
#' 
#' @seealso \code{\link{AD0}}
#'
#' @examples
#' ad0 <- AD0("2014-01-01T00")
#' as.character.AD0(ad0)
#' 
## @include
#' @export as.character.AD0
#' @docType methods
#' @rdname asCharAd0-methods
## @aliases
as.character.AD0 <- function(object) {
    return(substr(object$timestamp, 1, 10))
}

## @include
#' @export
setMethod("show", signature = "AD0",
          definition = function(object){
            print(as.character(object))
          })
