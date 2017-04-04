#' Calculate the 10-m winds for a vector of wind speed measurements according to Hsu et al. (1994)
#'
#' This function will return the 10-m winds given vectors of wind observations and the measurement height.
#' @param wspds vector containing the wind speed observations
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
#' ndbc10m(wspds, 10, inunits = "mph")
#'
#' # get potential winds from a station with anemometer height of 4.9 m
#' ndbc10m(wspds, 4.9)

ndbc10m <- function(wspds, zm, zref = 10.0, inunits = "m/s", outunits = "m/s",
                     to.na = TRUE, missing = -99999) {

    # change all large values to NA
    wspds[abs(wspds) > 300] <- NA

    # process the units accordingly
    wspds <- convert_units(wspds, inunits, outunits)

    # use Hsu et al. (1994) formula to convert winds to reference height
    wspds_10m <- wspds * (zref/zm) ^ 0.11

    # fill NAs with missing value if so specified
    if (to.na == FALSE) wspds_10m[is.na(wspds_10m)] <- missing

    return(wspds_10m)
}
