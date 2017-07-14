.fitCurve <- function (x,
                       y,
                       f,
                       density,
                       step,
                       precision,
                       lower_bounds,
                       upper_bounds,
                       scale,
                       family,
                       median_n,
                       trunc,
                       verbose,
                       gritty_guess,
                       span) {
  
  guess <- tryCatch(optim(par = gritty_guess,
                          fn = function(t) {.residual(x = x,
                                                      y = y,
                                                      n = median_n,
                                                      pars = guess,
                                                      f = f,
                                                      scale = scale,
                                                      family = family,
                                                      trunc = trunc)},
                          lower = lower_bounds,
                          upper = upper_bounds,
                          method = "L-BFGS-B"),
                    error = function(e) {list(par = gritty_guess, convergence = -1)})
  failed = guess[["convergence"]] != 0
  guess <- guess[["par"]]
  
  guess_residual <- .residual(x = x,
                              y = y,
                              n = median_n,
                              pars = guess,
                              f = f,
                              scale = scale,
                              family = family,
                              trunc = trunc)
  gritty_guess_residual <- .residual(x = x,
                                     y = y,
                                     n = median_n,
                                     pars = gritty_guess,
                                     f = f,
                                     scale = scale,
                                     family = family,
                                     trunc = trunc)
  
  if (failed || any(is.na(guess)) || guess_residual >= gritty_guess_residual) {
    guess <- .meshEval(x = x,
                       y = y,
                       f = f,
                       guess = gritty_guess,
                       lower_bounds = lower_bounds, 
                       upper_bounds = upper_bounds,
                       density = density, 
                       n = median_n,
                       family = family,
                       trunc = trunc)
    guess_residual <- .residual(x = x,
                                y = y,
                                n = median_n,
                                pars = sieve_guess,
                                f = f,
                                scale = scale, 
                                family = family,
                                trunc = trunc)
    
    guess <- .patternSearch(guess = guess,
                            guess_residual = guess_residual,
                            span = span,
                            precision = precision,
                            step = step,
                            f = f)
  }
  
  return(guess)
}