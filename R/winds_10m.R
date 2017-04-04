#' Calculate the 10-m winds for a vector of wind speed measurements
#'
#' This function will return the 10-m winds given vectors of wind observations (wind speed and direction), a vector containing the high end of the wind direction bins, the roughness lengths associated with each wind direction bins, and the measurement height of the wind observations.
#' @param bins vector containing the cutoffs of the wind direction bins
#' @param z0 vector containing the roughness lengths associated with each wind direction bin
#' @param wspds vector containing the wind speed observations
#' @param wdirs vector containing the wind direction observations
#' @param zm the measurement height for wind speed observations (in m)
#' @param zref the reference height (the height we want to estimate the winds), defaults to 10-m
#' @param inunits units for the wind observations, "m/s" (default), "mph", "knots"
#' @param outunits desired units for the potential winds, "m/s" (default), "mph", "knots"
#' @param to.na turn large values to NA (default = TRUE)
#' @param missing specify missing value (if to.na = FALSE)
#' @keywords wind potential
#' @export
#' @examples
#' # convert from mph to m/s
#' winds_10m(bins, z0, wspd, wdir, zm, inunits = "mph")

winds_10m <- function(bins, z0, wspds, wdirs, zm, zref = 10.0, inunits = "m/s",
                      outunits = "m/s", to.na = TRUE, missing = -99999) {

    # change all unrealistic values to NA
    wspds[abs(wspds) > 300] <- NA
    wdir[abs(wdir) > 360] <- NA

    # process the units accordingly
    wspds <- convert_units(wspds, inunits, outunits)

    # parameters for the loop
    z0_current <- NULL
    wspd_10m <- NULL
    wspds_10m <- NULL

    # loop through wspds and convert one at a time
    for (i in 1:length(wspds)) {
        if (is.na(wdirs[i])) wspd_10m <- NA
        else {
            z0_current <- z0[find_bin(bins, wdirs[i])]
            wspd_10m <- wspds[i] * log(zref/z0_current) / log(zm/z0_current)
        }
        wspds_10m = c(wspds_10m, wspd_10m)
    }

    # fill NAs with missing value if so specified
    if (to.na == FALSE) wspds_10m[is.na(wspds_10m)] <- missing

    return(wspds_10m)
}
