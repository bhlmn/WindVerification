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

# calculate the eucledian norm
getenorm <- function(x1, x2, y1, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}
