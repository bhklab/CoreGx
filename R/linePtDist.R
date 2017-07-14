linePtDist <- function(x, #x-coordinate of point
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