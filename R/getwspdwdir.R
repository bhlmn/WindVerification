#' Convert u and v wind components into wind speed and wind direction
#'
#' Convert u and v wind components into wind speed and wind direction
#' @param u zonal wind component
#' @param v meridional wind component
#' @keywords wind u v speed direction convert
#' @export
#' @examples
#' u <- -4.56
#' v <- 147
#' wspdwdir <- getwspdwdir(wspd, wdir)

# get the wind speed and direction from u and v components
getwspdwdir <- function(u, v) {

    # If either u or v are missing, we cannot do the calculation!
    if (anyNA(c(u, v))) {return(c(NA, NA))}

    # Calculate wind speed and wind direction
    wspd <- sqrt(u^2 + v^2)
    wdir <- atan2(-1 * u, -1 * v) * 57.2957795131 # 57.2957795131 is 180 / pi

    # atan2 goes from -pi to pi, so you need to add 360 if negative
    if (wdir < 0) {wdir <- wdir + 360}
    if (round(wdir) >= 360) {wdir <- 0}

    # return wind speed first, then wind direction rounded to nearest integer
    return(c(wspd, round(wdir)))
}
