#' @importFrom stats optim
#' @importFrom stats var
#' @export
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
                                                      pars = t,
                                                      f = f,
                                                      scale = scale,
                                                      family = family,
                                                      trunc = trunc)},
                          lower = lower_bounds,
                          upper = upper_bounds,
                          control = list(factr = 1e-8, trace=0),
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
                       scale = scale,
                       family = family,
                       trunc = trunc)
    guess_residual <- .residual(x = x,
                                y = y,
                                n = median_n,
                                pars = guess,
                                f = f,
                                scale = scale, 
                                family = family,
                                trunc = trunc)
    
    guess <- .patternSearch(x = x,
                            y = y,
                            f = f,
                            guess = guess,
                            n = median_n,
                            guess_residual = guess_residual,
                            lower_bounds = lower_bounds,
                            upper_bounds = upper_bounds,
                            span = span,
                            precision = precision,
                            step = step,
                            scale = scale,
                            family = family,
                            trunc = trunc)
  }
  
  y_hat <- do.call(f, list(x, guess))

  Rsqr <- 1 - var(y - y_hat)/var(y)
  attr(guess, "Rsquare") <- Rsqr

  return(guess)
}
