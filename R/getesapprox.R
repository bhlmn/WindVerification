#' Calculate the two dimensional Eucledian norm
#'
#' Calculates the two dimensional Eucledian norm between two points.
#' @param x1 point 1 x component
#' @param x2 point 2 x component
#' @param y1 point 1 y component
#' @param y2 point 2 y component
#' @keywords eucledian normal wind verification
#' @export
#' @examples
#' wspdwdir <- getwspdwdir(wspd, wdir)

require(MASS) # sampling from bivariate normal distribution

# calculate the energy score given the EMOS parameters
getesapprox <- function(u, v, mu.u, mu.v, s.u, s.v, rho, n = 10000) {

    # check to see if there are any missing arguments
    if (anyNA(c(u, v, mu.u, mu.v, s.u, s.v, rho))) {return(NA)}

    # First, we need to generate some samples from the distribution specified
    # by these parameters
    mus <- c(mu.u, mu.v)
    sigmas <- sigma <- matrix(c(s.u^2, s.u*s.v*rho, s.u*s.v*rho, s.v^2), 2)
    rndm.sample <- mvrnorm(n, mu = mus, Sigma = sigmas)

    # now that we have our sample, calculate the energy score
    # left hand side is simple
    lhs <- mean(mapply(calcENorm, rndm.sample[,1], rndm.sample[,2],
                       MoreArgs = list(x = u, y = v)))

    # right hand side needs a loop
    rhs.sum <- 0
    for (i in 1:(n-1)) {
        rhs.sum <- rhs.sum + calcENorm(rndm.sample[i, 1], rndm.sample[i, 2],
                                       rndm.sample[i+1, 1], rndm.sample[i+1, 2])
    }
    rhs <- 1/(2 * (n - 1)) * rhs.sum
    return(lhs - rhs)
}
