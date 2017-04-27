#' Calculate the multivariate rank given vectors of u and v ensemble forecasts
#'
#' Calculate the multivariate rank given vectors of u and v ensemble forecasts
#' @param u u-component of verifying wind observation
#' @param v v-component of verifying wind observation
#' @param mu.u u-component of EMOS mean wind vector
#' @param mu.v v-component of EMOS mean wind vector
#' @param s.u square root of u-component of EMOS variance
#' @param s.v square root of v-component of EMOS variance
#' @param rho u/v correlation coefficient
#' @param n number of ensemble members
#' @return numeric ... the raw ensemble multivariate rank
#' @author Bryan Holman
#' @keywords multivariate rank wind verification
#' @export
#' @examples

#' # calculate multivariate rank
#' emosmvrank <- getemosmvrank(u, v, mu.u, mu.v, s.u, s.v)

# calculate the multivariate rank
getemosmvrank <- function(u, v, mu.u, mu.v, s.u, s.v, rho = 0, n = 21) {

    # check to see if there are any missing arguments
    if (anyNA(c(u, v, mu.u, mu.v, s.u, s.v, rho))) {return(NA)}

    # First, we need to generate some samples from the distribution specified
    # by these parameters
    mus <- c(mu.u, mu.v)
    sigmas <- sigma <- matrix(c(s.u^2, s.u*s.v*rho, s.u*s.v*rho, s.v^2), 2)
    rndm.sample <- MASS::mvrnorm(n, mu = mus, Sigma = sigmas)

    # combine the obs and forecasts
    df.MVRank <- data.frame(us = c(u, rndm.sample[,1]),
                            vs = c(v, rndm.sample[,2]))

    # create a vector to store the counts, the count for each u/v pair is equal
    # to the number of pairs in the combined obs/forecasts that are to the
    # lower left in u/v cartesian space
    n <- length(df.MVRank$us)
    counts <- rep(NA, n)

    # determine counts
    for (i in 1:n) {
        count <- 0
        for (j in 1:n) {
            if (i == j) {next}
            if (df.MVRank$us[j] < df.MVRank$u[i] &
                df.MVRank$vs[j] < df.MVRank$vs[i]) {count <- count + 1}
        }
        counts[i] <- count
    }

    # add counts to the dataframe
    df.MVRank$count <- counts
    rm(count, counts)

    # randomize the dataframe and then sort by count, this promotes a stochastic
    # result since when I initialize the dataframe the ob is always at the top.
    # without sampling, the ob would always get the lowest possible rank. This
    # fixes that
    df.MVRank <- df.MVRank[sample(1:n),]
    df.MVRank <- df.MVRank[order(df.MVRank$count),]

    # assign the ranks to the sorted dataframe, then return the rank of the
    # observation
    df.MVRank$rank <- 1:n
    rank <- df.MVRank[df.MVRank$us == u & df.MVRank$vs == v,]$rank
    return(rank)
}
