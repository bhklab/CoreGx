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
                          fn = function(t) {.residual(D,
                                                      SF,
                                                      pars = t,
                                                      n = median_n,
                                                      scale = scale,
                                                      family = family,
                                                      trunc = trunc)},
                          lower = lower_bounds,
                          upper = upper_bounds,
                          method = "L-BFGS-B"),
                    error = function(e) {list(par = gritty_guess, convergence = -1)})
  failed = guess[["convergence"]] != 0
  guess <- guess[["par"]]
  
  guess_residual <- .residual(D,
                              SF,
                              pars = guess, 
                              n = median_n,
                              scale = scale,
                              family = family,
                              trunc = trunc)
  gritty_guess_residual <- .residual(log_conc,
                                     viability,
                                     pars = gritty_guess, 
                                     n = median_n,
                                     scale = scale,
                                     family = family,
                                     trunc = trunc)
  
  if (failed || any(is.na(guess)) || guess_residual >= gritty_guess_residual) {
    guess <- .meshEval(D,
                       SF,
                       lower_bounds = lower_bounds, 
                       upper_bounds = upper_bounds,
                       density = density, 
                       n = median_n,
                       scale = scale,
                       family = family,
                       trunc = trunc)
    guess_residual <- .residual(D,
                                SF, 
                                pars = sieve_guess,
                                n = median_n,
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