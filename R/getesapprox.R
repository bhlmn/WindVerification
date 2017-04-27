#' Calculate the approximate energy score given EMOS parameters
#'
#' Calculate the approximate energy score given EMOS parameters
#' @param u u-component of verifying wind observation
#' @param v v-component of verifying wind observation
#' @param mu.u u-component of EMOS mean wind vector
#' @param mu.v v-component of EMOS mean wind vector
#' @param s.u square root of u-component of EMOS variance
#' @param s.u square root of v-component of EMOS variance
#' @param rho u/v correlation coefficient
#' @param n number of distribution samples used to approximate energy score
#' @return numeric ... the approximate energy score
#' @author Bryan Holman
#' @keywords energy score wind verification
#' @export
#' @examples
#' NULL

# calculate the energy score given the EMOS parameters
getesapprox <- function(u, v, mu.u, mu.v, s.u, s.v, rho = 0, n = 10000) {

    # check to see if there are any missing arguments
    if (anyNA(c(u, v, mu.u, mu.v, s.u, s.v, rho))) {return(NA)}

    # First, we need to generate some samples from the distribution specified
    # by these parameters
    mus <- c(mu.u, mu.v)
    sigmas <- matrix(c(s.u^2, s.u*s.v*rho, s.u*s.v*rho, s.v^2), 2)
    rndm.sample <- MASS::mvrnorm(n, mu = mus, Sigma = sigmas)

    # now that we have our sample, calculate the energy score
    # left hand side is simple
    lhs <- mean(mapply(getenorm, rndm.sample[,1], rndm.sample[,2],
                       MoreArgs = list(x2 = u, y2 = v)))

    # right hand side needs a loop
    rhs.sum <- 0
    for (i in 1:(n-1)) {
        rhs.sum <- rhs.sum + getenorm(rndm.sample[i, 1], rndm.sample[i, 2],
                                      rndm.sample[i+1, 1], rndm.sample[i+1, 2])
    }
    rhs <- 1/(2 * (n - 1)) * rhs.sum
    return(lhs - rhs)
}
