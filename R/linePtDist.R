#' Calculate shortest distance between point and line
#'
#'
#' @examples distancePointLine(0, 0, 1, -1, 1)
#'
#' @description This function calculates the shortest distance between a point and a line in 2D space.
#' @param x x-coordinate of point
#' @param y y-coordinate of point
#' @param a coefficient in line equation a * x + b * y + c = 0
#' @param b coefficient in line equation a * x + b * y + c = 0
#' @param c coefficient in line equation a * x + b * y + c = 0
#' @export

distancePointLine <- function(x, y, a, b, c) {
  
  if (!(all(is.finite(c(x, y, a, b, c))))) {
    stop("All inputs to linePtDist must be real numbers.")
  }
  
  return(abs(a * x + b * y + c) / sqrt(a ^ 2 + b ^ 2))
}

.magnitude <- function(p1, p2) {
  return (sqrt(sum((p2 - p1) ^ 2)))
}

#' Calculate shortest distance between point and line segment
#'
#' @description This function calculates the shortest distance between a point and a line segment in 2D space.
#' @param x x-coordinate of point
#' @param y y-coordinate of point
#' @param x1 x-coordinate of one endpoint of the line segment
#' @param y1 y-coordinate of line segment endpoint with x-coordinate x1
#' @param x2 x-coordinate of other endpoint of line segment
#' @param y2 y-coordinate of line segment endpoint with x-coordinate x2
#' @examples distancePointSegment(0, 0, -1, 1, 1, -1)
#' @export

distancePointSegment <- function(x, #x-coordinate of point
                                 y, #y-coordinate of point
                                 x1, #x-coordinate of one endpoint of line segment
                                 y1, #y-coordinate of line segment endpoint with x-coordinate x1
                                 x2, #x-coordinate of other endpoint of line segment,
                                 y2) { #y-coordinate of line segment endpoint with x-coordinate x2
  if (!(all(is.finite(c(x, y, x1, x2, y1, y2))))) {
    stop("All inputs to linePtDist must be real numbers.")
  }
  
  bestEndpointDistance <- min(c(.magnitude(c(x, y), c(x1, y1)),
                                .magnitude(c(x, y), c(x2, y2))))
  
  if (.magnitude(c(x, y), c((x1 + x2) / 2, (y1 + y2) / 2)) < bestEndpointDistance) { # length to point has only one local minimum which is the global minimum; iff this condition is true then the shortest distance is to a point on the line segment
    if (x1 == x2) { #vertical line segment
      a <- 1
      b <- 0
      c <- -x1
    } else {
      a <- (y2 - y1) / (x2 - x1)
      b <- -1
      c <- y1 - a * x1
    }
    return(distancePointLine(x, y, a, b, c))
  } else {
    return(bestEndpointDistance)
  }
}