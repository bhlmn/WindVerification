#' Calculate the bivariate absolute error for a forecast/observation wind vector pair
#'
#' Calculate the bivariate absolute error for a forecast/observation wind vector pair. Missing values are not allowed and will be returned with a vector of four NAs.
#' @param ob.u observed u component of wind
#' @param ob.v observed v component of wind
#' @param fcst.u forecast u component of wind
#' @param fcst.v forecast v component of wind
#' @keywords wind bivariate absolute error verification
#' @export
#' @examples
#' getbae(ob.u, ob.v, fcst.u, fcst.v)

# returns the bivariate absolute error when given observed u, observed v,
# forecasted u, and forecasted v
getbae <- function(ob.u, ob.v, fcst.u, fcst.v) {

    # If any information is missing, we cannot do the calculation!
    if (anyNA(c(ob.u, ob.v, fcst.u, fcst.v))) return(c(NA, NA, NA, NA))

    # else return the bAE
    return(sqrt((fcst.u - ob.u)^2 + (fcst.v - ob.v)^2))
}
