#' Calculate the multivariate rank given vectors of u and v ensemble forecasts
#'
#' Calculate the multivariate rank given vectors of u and v ensemble forecasts
#' @param ob.u u-component of verifying wind observation
#' @param ob.v v-component of verifying wind observation
#' @param ens.us u-components of ensemble wind forecasts
#' @param ens.vs v-components of ensemble wind forecasts
#' @return numeric ... the raw ensemble multivariate rank
#' @author Bryan Holman
#' @keywords multivariate rank wind verification
#' @export
#' @examples
#' # the verifying observation
#' u <- 4.5
#' v <- -3.6
#'
#' # synthetic ensembles
#' ens.us <- c(4.6, 3.5, 2.9, 5.3)
#' ens.vs <- c(-4.8, -2.1, -3.9, -3.0)
#'
#' # calculate multivariate rank
#' mvrank <- getmvrank(u, v, ens.us, ens.vs)

# calculate the multivariate rank
getmvrank <- function(ob.u, ob.v, ens.us, ens.vs) {

    # combine the obs and forecasts
    df.mvrank <- data.frame(us = c(ob.u, ens.us), vs = c(ob.v, ens.vs))

    # create a vector to store the counts, the count for each u/v pair is equal
    # to the number of pairs in the combined obs/forecasts that are to the
    # lower left in u/v cartesian space
    n <- length(df.mvrank$us)
    counts <- rep(NA, n)

    # determine counts
    for (i in 1:n) {
        count <- 0
        for (j in 1:n) {
            if (i == j) {next}
            if (df.mvrank$us[j] < df.mvrank$u[i] &
                df.mvrank$vs[j] < df.mvrank$vs[i]) {count <- count + 1}
        }
        counts[i] <- count
    }

    # add counts to the dataframe
    df.mvrank$count <- counts
    rm(count, counts)

    # randomize the dataframe and then sort by count, this promotes a stochastic
    # result since when I initialize the dataframe the ob is always at the top.
    # without sampling, the ob would always get the lowest possible rank. This
    # fixes that
    df.mvrank <- df.mvrank[sample(1:n),]
    df.mvrank <- df.mvrank[order(df.mvrank$count),]

    # assign the ranks to the sorted dataframe, then return the rank of the
    # observation
    df.mvrank$rank <- 1:n
    rank <- df.mvrank[df.mvrank$us == ob.u & df.MVRank$vs == v10,]$rank
    return(rank)
}
