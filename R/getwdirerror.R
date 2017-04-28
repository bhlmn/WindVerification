#' Calculate the wind direction error of a forecast/observation pair
#'
#' Calculate the wind direction error of a forecast/observation pair
#' @param ob.wdir observed wind direction
#' @param fcst.wdir forecasted wind direction
#' @keywords wind direction error
#' @export
#' @examples
#' ob.wdir <- 63
#' fcst.wdir <- 147
#' wdirerror <- getwdirerror(wspd, wdir)

# returns the wind direction error
getwdirerror <- function(ob.wdir, fcst.wdir) {
    # make sure inputs contain no missing values
    if (anyNA(c(ob.wdir, fcst.wdir))) return(NA)
    # make sure inputs are in the range of [0, 360)
    if (ob.wdir < 0 | ob.wdir >= 360 | fcst.wdir < 0 | fcst.wdir >= 360) {
        return(NA)
    }
    # to account for 0/360 one-ness, calculate 3 different wdir errors and
    # return the minimum of the absolute value of the three
    err.1 <- fcst.wdir - ob.wdir
    err.2 <- (fcst.wdir + 360) - ob.wdir
    err.3 <- fcst.wdir - (ob.wdir + 360)
    if (abs(err.1) == min(abs(c(err.1, err.2, err.3)))) return(err.1)
    if (abs(err.2) == min(abs(c(err.1, err.2, err.3)))) return(err.2)
    if (abs(err.3) == min(abs(c(err.1, err.2, err.3)))) return(err.3)
}
