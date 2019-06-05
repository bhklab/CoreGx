#' @export
.patternSearch <- function(x, y, f, guess, n, guess_residual, lower_bounds, upper_bounds, span, precision, step, scale, family, trunc) {
  neighbours <- matrix(nrow = 2 * length(guess), ncol = length(guess))
  neighbour_residuals <- matrix(NA, nrow = 1, ncol = nrow(neighbours))
  
  while (span > precision) {
    for (neighbour in 1:nrow(neighbours)) {
      neighbours[neighbour, ] <- guess
      dimension <- ceiling(neighbour / 2)
      if (neighbour %% 2 == 1) {
        neighbours[neighbour, dimension] <- pmin(guess[dimension] + span * step[dimension], upper_bounds[dimension])
      } else {
        neighbours[neighbour, dimension] <- pmax(guess[dimension] - span * step[dimension], lower_bounds[dimension])
      }
      
      neighbour_residuals[neighbour] <- .residual(x = x, 
                                                  y = y,
                                                  f = f,
                                                  pars = neighbours[neighbour, ],
                                                  n = n, 
                                                  scale = scale,
                                                  family = family,
                                                  trunc = trunc)
    }
    
    if (min(neighbour_residuals) < guess_residual) {
      guess <- neighbours[which.min(neighbour_residuals)[1], ]
      guess_residual <- min(neighbour_residuals)
    } else {
      span <- span / 2
    }
  }
  
  return(guess)
}