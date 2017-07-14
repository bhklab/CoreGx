distancePointLine <- function(x, #x-coordinate of point
                              y, #y-coordinate of point
                              a, #coefficient in line equation ax + by + c = 0
                              b, #coefficient in line equation ax + by + c = 0
                              c) { #coefficient in line equation ax + by + c = 0
  
  #Function calculates shortest distance between point and line in R^2.
  
  if (!(all(is.finite(c(x, y, a, b, c))))) {
    stop("All inputs to linePtDist must be real numbers.")
  }
  
  return(abs(a * x + b * y + c) / sqrt(a ^ 2 + b ^ 2))
}

.magnitude <- function(p1, p2) {
  return (sqrt(sum((p2 - p1) ^ 2)))
}

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
  
  if (.magnitude(c(x, y), c((x1 + x2) / 2, (y1 + y2) / 2)) < bestEndpointDistance) {
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