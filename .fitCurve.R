.fitCurve <- function (x,
                       y,
                       density,
                       step,
                       precision,
                       lower_bounds,
                       upper_bounds,
                       scale,
                       family,
                       median_n,
                       trunc,
                       verbose) {
  match.arg(family)
  
  if (!SF_as_log) {
    SF <- log(SF)
  }
  
  if (trunc) {
    SF[which(SF > 0)] <- 0
  }
  
  guess <- tryCatch(optim(par = gritty_guess,
                          fn = function(x) {.residual(D,
                                                      SF,
                                                      pars = x,
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
    sieve_guess <- .meshEval(D,
                             SF,
                             lower_bounds = lower_bounds, 
                             upper_bounds = upper_bounds,
                             density = density, 
                             n = median_n,
                             scale = scale,
                             family = family,
                             trunc = trunc)
    sieve_guess_residual <- .residual(D,
                                      SF, 
                                      pars = sieve_guess,
                                      n = median_n,
                                      scale = scale, 
                                      family = family,
                                      trunc = trunc)
    guess <- sieve_guess
    guess_residual <- sieve_guess_residual
    
    guess <- .patternSearch(guess = sieve_guess,
                            guess_residual = sieve_guess_residual,
                            span = 0.1,
                            precision = precision,
                            step = step,
                            f = f)
  }
  
  return(list(alpha = guess[1], beta = guess[2]))
}