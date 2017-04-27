#' Calculate the two dimensional Eucledian norm
#'
#' Calculates the two dimensional Eucledian norm between two points.
#' @param x1 point 1 x component
#' @param y1 point 1 y component
#' @param x2 point 2 x component
#' @param y2 point 2 y component
#' @keywords eucledian normal wind verification
#' @return numeric ... the eecledian norm
#' @author Bryan Holman
#' @export
#' @examples
#' point1 <- c(2, 5)
#' point2 <- c(7, 9)
#'
#' # calculate eucledian norm between two points
#' enorm <- getenorm(point1[1], point1[2], point2[1], point2[2])

# calculate the eucledian norm
getenorm <- function(x1, y1, x2, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}
