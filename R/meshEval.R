.meshEval <- function (x, y, f, guess, lower_bounds, upper_bounds, density, n, scale, family, trunc) {
  pars <- NULL
  guess_residual <- .residual(x = x,
                              y = y,
                              n = n,
                              pars = guess,
                              f = f,
                              scale = scale,
                              family = family,
                              trunc = trunc)
  
  periods <- matrix(NA, nrow = length(guess), ncol = 1)
  names(periods) <- names(guess)
  periods[1] <- 1
  
  if (length(guess) > 1) {
    for (par in 2:length(guess)) {
      periods[par] <- periods[par - 1] * density[par] * (upper_bounds[par] - lower_bounds[par])
    }
  }
  
  currentPars <- lower_bounds
  for (point in 1:prod((upper_bounds - lower_bounds) * density)) {
    for (par in seq_along(guess)) {
      if (point %% periods[par] == 0) {
        if (currentPars[par] >= upper_bounds[par]) {
          currentPars[par] <- lower_bounds[par]
        } else {
          currentPars[par] <- currentPars[par] + 1 / density[par]
        }
      }
    }
    test_guess_residual <- .residual(x = x,
                                     y = y,
                                     n = n,
                                     pars = currentPars,
                                     f = f,
                                     scale = scale,
                                     family = family,
                                     trunc = trunc)
    if (!length(test_guess_residual) || (!is.finite(test_guess_residual) && test_guess_residual != Inf)) {
      stop(paste0(" Test Guess Residual is: ", test_guess_residual, "\n",
                     "Other Pars:\n", "x: ", paste(x, collapse = ", "), "\n",
                     "y: ", paste(y, collapse = ", "), "\n",
                     "n: ", n, "\n",
                     "pars: ", pars, "\n",
                     "scale: ", scale, "\n",
                     "family : ", family, "\n",
                     "Trunc ", trunc))
    }
    if (test_guess_residual < guess_residual) {
      guess <- currentPars
      guess_residual <- test_guess_residual
    }
  }
  
  return(guess)
}