#' Calculate the energy score given vectors of u and v ensemble forecasts
#'
#' Calculate the energy score given vectors of u and v ensemble forecasts
#' @param ob.u u-component of verifying wind observation
#' @param ob.v v-component of verifying wind observation
#' @param ens.us u-components of ensemble wind forecasts
#' @param ens.vs v-components of ensemble wind forecasts
#' @return numeric ... the raw ensemble energy score
#' @author Bryan Holman
#' @keywords energy score wind verification
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
#' # calculate energy score
#' es <- getes(u, v, ens.us, ens.vs)

# calculate the energy score given the EMOS parameters
getes <- function(ob.u, ob.v, ens.us, ens.vs) {

    # Make sure that we have observations and a full ensemble
    if (anyNA(c(ob.u, ob.v, ens.us, ens.vs))) {return(NA)}

    # calculate left hand side
    lhs <- mean(mapply(getenorm, ens.us, ens.vs,
                       MoreArgs = list(x2 = ob.u, y2 = v10.row)))

    # calculate right hand side
    rhs.sum <- 0
    for (i in 1:length(ens.us)) {
        for (j in 1:length(ens.vs)) {
            rhs.sum <- rhs.sum + getenorm(ens.us[i], ens.vs[i],
                                          ens.us[j], ens.vs[j])
        }
    }
    rhs <- rhs.sum * (1 / (2 * (length(ens.us))^2))
    return(lhs - rhs)
}
