#' Calculate bivariate mean absolute error over many forecast/observation wind vector pairs
#'
#' Calculate bivariate mean absolute error over many forecast/observation wind vector pairs.
#' @param obs.u vector containing observed u wind components
#' @param obs.v vector containing observed v wind components
#' @param fcsts.u vector containing forecast u wind components
#' @param fcsts.v vector containing forecast v wind components
#' @keywords wind bivariate absolute error verification
#' @export
#' @examples
#' getbmae(obs.u, obs.v, fcsts.u, fcsts.v)

# returns the bivariate mean absolute error when given vectors of equal length
# that contain observed us, observed vs, forecasted us, and forecasted vs
getbmae <- function(obs.u, obs.v, fcsts.u, fcsts.v) {

    # if these vectors are of different lengths, return NAs
    if (length(obs.u) != length(obs.v) |
        length(obs.u) != length(fcsts.u) |
        length(obs.u) != length(fcsts.v)) {
        warning('Input observation/forecast vectors have different lengths! Returning NAs')
        return(c(NA, NA, NA))
    }

    # calculate the bAE for each row in the provided dataframes
    bae.vec <- mapply(getbae, obs.u, obs.v, fcsts.u, fcsts.v)

    # get the number of NAs and the total length
    lngth <- length(bae.vec)
    num.NA <- sum(is.na(bae.vec))

    # return the mean of this vector, the number of NAs, and the vector length
    return(c(mean(bae.vec, na.rm = TRUE), round(num.NA, 0), round(lngth)))
}
