#' Convert wind speeds from one unit to another
#'
#' Given a vector of wind speed data, this script applies a multiplier to convert from one type of measurement unit to another. Missing values are allowed.
#' @param wspds vector containing the wind speed observations
#' @param inunits units for the wind observations, "m/s" (default), "mph", "knots"
#' @param outunits desired units for the potential winds, "m/s" (default), "mph", "knots"
#' @keywords wind units convert
#' @export
#' @examples
#' # convert from mph to m/s
#' convert_units(wspds, inunits = "mph")

convert_units <- function(wspds, inunits = 'm/s', outunits = 'm/s') {

    # only do a conversion if the units are not the same
    if (inunits != outunits) {

        # convert from m/s ...
        if (inunits == 'm/s') {

            # ... to mph
            if (outunits == 'mph') wspds <- wspds * 2.23694

            # ... to knots
            if (outunits == 'knots') wspds <- wspds * 1.94384
        }

        # convert from mph ...
        if (inunits == 'mph') {

            # ... to m/s
            if (outunits == 'm/s') wspds <- wspds * 0.44704

            # ... to knots
            if (outunits == 'knots') wspds <- wspds * 0.868976
        }

        # convert from knots ...
        if (inunits == 'knots') {

            # ... to m/s
            if (outunits == 'm/s') wspds <- wspds * 0.51444

            # ... to mph
            if (outunits == 'mph') wspds <- wspds * 1.15078
        }
    }

    return(wspds)
}
