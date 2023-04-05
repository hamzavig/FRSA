#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************


##############################################################
#' Generic function to calculate year fractions
#'
#' This function takes two date arrays and an optional input
#' for the daycount convention and calculates year fractions
#' accordingly.
#' 
#' @param start_dates Array of dates from which to 
#'        calculate year fractions in format "yyyy-mm-dd".
#'        
#' @param end_dates Array of dates until which to 
#'        calculate year fractions in format "yyyy-mm-dd".
#' 
#' @param ... Currently unused
#'
#' @return numeric Array of year fraction calculated from 
#'         date vectors
#' 
#' @examples
#' yearFraction("2020-01-01","2020-01-31","30E/360")
#' yearFraction(c("2020-01-01","2020-01-31"),"2020-01-31")
#' yearFraction("2020-01-01",c("2020-01-01","2020-01-31"))
#' yearFraction(c("2020-01-01","2020-01-31"),c("2020-01-31","2020-02-28"))
#' 
#' @export
#' @rdname yfrc-methods
#' @aliases yearFraction, charachter, charachter-method
setGeneric(name = "yearFraction",
           def = function(start_dates, end_dates, ...){
             standardGeneric("yearFraction")
           })

#' @export
setMethod(f = "yearFraction", signature = c("character", "character"),
          definition = function(start_dates, end_dates, convention = "30E360", ...){
            
            # test if date inputs are of valid format
            test.dates.yearFraction(start_dates, end_dates)
            
            # convert ACTUS convention to the valid 'fmdates' convention
            fmdates_conv <- map.conventions.yearFraction(convention)
            
            # calculate year fraction
            frac <- year_frac(ymd(start_dates), ymd(end_dates), fmdates_conv)
            return(frac)
          })


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# yearFraction - Helper Function
# test if dates inputs are appropriately set

test.dates.yearFraction <- function(start_dates, end_dates){
  
  # check if dates come in convertible format
  tryCatch({
    as.Date(start_dates)
    as.Date(end_dates)
  }, error = function(e) {
    stop("ErrorIn::yearFraction:: Dates need to be in Date format !!!")
  })
  
  # check if length of date vectors are valid
  if ((length(start_dates) > 1) & (length(end_dates) > 1)) {
    if (length(start_dates) != length(end_dates)) {
      stop("ErrorIn::yearFraction:: Array lengths for ' start_dates ' and ' end_dates ' are not valid !!!")
    }
  }

}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# yearFraction - Helper Function
# map the ACTUS daycount conventions to the allowed values from 
# 'fmdates' package

map.conventions.yearFraction <- function(actus_conv) {
  
  # Note: ACTUS convention B/252 and <wildcard> is not supported !!!
  allowed_convs <- c("30e/360", "30e/360isda", "act/360", "act/365","act/actisda")
  names(allowed_convs) <- c("30E360", "30E360ISDA", "A360", "A365", "AA")
  
  # all possible values in the function year_frac are the following:
  # "30/360", "30/360us", "30e/360", "30e/360isda", "30e+/360", 
  # "act/360", "act/365","act/actisda")
  
  if(actus_conv %in% names(allowed_convs)) {
    conv <- allowed_convs[[actus_conv]]
  } else {
    stop(paste("ErrorIn::yearFraction:: ", actus_conv, " is not a valid ACTUS convention !!!", sep=" "))
  }
  return(conv)
}










