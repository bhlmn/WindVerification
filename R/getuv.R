#' Convert wind speed and wind direction into u and v wind components
#'
#' Convert wind speed and wind direction into u and v wind components
#' @param wspd wind speed value
#' @param wdir wind direction value
#' @keywords wind u v speed direction convert
#' @export
#' @examples
#' wspd <- 6.0
#' wdir <- 147
#' uv <- getuv(wspd, wdir)

# function to turn wspd and wdir into a vector of u and v components
getuv <- function(wspd, wdir) {

    # If either wspd or wdir are missing, we cannot do the calculation!
    if (is.na(wspd) | is.na(wdir)) {return(c(NA, NA))}

    # calculate u and v
    u <- -1 * wspd * sin(0.01745329251 * wdir) # 0.01745329251 is pi / 180
    v <- -1 * wspd * cos(0.01745329251 * wdir)

    # round off floating point errors
    if (u < 0.0001 & u > -0.0001) {u <- 0}
    if (v < 0.0001 & v > -0.0001) {v <- 0}

    return(c(u, v))
}
