#' Calculate the 10-m winds for a vector of wind speed measurements according to Hsu et al. (1994)
#'
#' This function will return the 10-m winds given vectors of wind observations and the measurement height.
#' @param wspd vector containing the wind speed observations
#' @param zm the measurement height for wind speed observations (in m)
#' @param zref the reference height (the height we want to estimate the winds), defaults to 10-m
#' @param inunits units for the wind observations, "m/s" (default), "mph", "knots"
#' @param outunits desired units for the potential winds, "m/s" (default), "mph", "knots"
#' @param to.na turn large values to NA (default = TRUE)
#' @param missing specify missing value (if to.na = FALSE)
#' @keywords wind potential wind
#' @export
#' @examples
#' # convert from mph to m/s
#' ndbc_10m(wspd, 10, inunits = "mph")
#' 
#' # get potential winds from a station with anemometer height of 4.9 m
#' ndbc_10m(wspd, 4.9)

ndbc_10m <- function(wspd, zm, zref = 10.0, inunits = "m/s", outunits = "m/s", 
                     to.na = TRUE, missing = -99999) {
    
    # change all large values to NA
    wspd[abs(wspd) > 300] <- NA
    
    # process the units accordingly
    if (inunits != outunits) {
        if (inunits == "m/s") {
            if (outunits == "mph") wspd <- wspd * 2.23694
            if (outunits == "knots") wspd <- wspd * 1.94384
        }
        if (inunits == "mph") {
            if (outunits == "m/s") wspd <- wspd * 0.44704
            if (outunits == "knots") wspd <- wspd * 0.868976
        }
        if (inunits == "knots") {
            if (outunits == "m/s") wspd <- wspd * 0.51444
            if (outunits == "mph") wspd <- wspd * 1.15078
        }
    }
    
    # use Hsu et al. (1994) formula to convert winds to reference height
    wspd_10m <- wspd * (zref/zm) ^ 0.11

    if (to.na == FALSE) wspd_10m[is.na(wspd_10m)] <- missing
    return(wspd_10m)
}