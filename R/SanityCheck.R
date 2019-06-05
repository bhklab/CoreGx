#' @export
.sanitizeInput <- function(x,
                           y,
                           lower,
                           upper,
                           pars,
                           x_as_log,
                           y_as_log,
                           y_as_pct,
                           trunc,
                           verbose = FALSE) { # Set to 2 to see debug printouts

  if (!is.logical(x_as_log)) {
    if (verbose == 2) {
      print("x_as_log:")
      print(x_as_log)
    }
    stop("'x_as_log' is not a logical.")
  }

  if (!is.logical(y_as_log)) {
    if (verbose == 2) {
      print("y_as_log:")
      print(y_as_log)
    }
    stop("'y_as_log' is not a logical.")
  }

  if (!is.logical(y_as_pct)) {
    if (verbose == 2) {
      print("y_as_pct:")
      print(y_as_pct)
    }
    stop("'y_as_pct' is not a logical.")
  }

  if (!is.logical(trunc)) {
    if (verbose == 2) {
      print("trunc:")
      print(trunc)
    }
    stop("'trunc' is not a logical.")
  }

  if (y_as_pct && y_as_log) {
    if (verbose == 2) {
      print("y_as_pct:")
      print(y_as_pct)
      print("y_as_log:")
      print(y_as_log)
    }
    warning("y_as_pct and y_as_log flags should almost certainly not both be TRUE.")
  }

  if(!(verbose %in% c(0, 1, 2))){
    print("verbose:")  #can't have the if(verbose == 2) statement here since verbose itself is the problem!
    print(verbose)
    stop("'verbose' flag is not set correctly.")
  }
  
  if (!missing(x)) {
    if (!all(is.finite(x) || is.na(x) || (x_as_log && x == -Inf))) {
      if (verbose == 2) {
        print("x:")
        print(x)
      }
      stop("x must contain only real numbers, NA-values, and/or -Inf (if x_as_log flag is set to TRUE).")
    }
    
    if (x_as_log == FALSE && min(x) < 0) {
      if (verbose == 2) {
        print("x:")
        print(x)
        print("x_as_log:")
        print(x_as_log)
      }
      stop("Negative x-values encountered. Data may be inappropriate, or 'x_as_log' flag may be set incorrectly.")
    }
    if(length(unique(x)) < 3){
      stop("Please pass in at least 3 unique dose points.")
    }
  }

  if (missing(y)) {
    if (missing(pars)) {
      stop("Both 'pars' and 'y' missing, please pass in some data!")
    } else {

      if (pars[[1]] < 0 || pars[[2]] < 0) { #HS or alpha
        if (verbose == 2) {
          print("pars:")
          print(pars)
        }
        warning("Curve parameters may be inappropriately set to negative values.")
      }

      if (length(pars) == 3) { #and thus we are in PharmacoGx
        if (x_as_log == FALSE && pars[[3]] < 0) {
          print("pars:")
          print(pars[[3]])
          print("x_as_log:")
          print(x_as_log)
          stop("'x_as_log' flag may be set incorrectly, as the EC50 is negative when a positive value is expected.")
        }

        if (y_as_pct == FALSE) {
          if (pars[[2]] > 1) {
            if (verbose == 2) {
              print("pars:")
              print(pars[[2]])
              print("y_as_pct:")
              print(y_as_pct)
            }
            warning("Warning: 'y_as_pct' flag may be set incorrectly.")
          }
        }
      } else if (length(pars) == 2) {
        if (pars[[1]] < pars[[2]]) {
          if (verbose) {
            warning("Alpha is greater than beta.")
            if (verbose == 2) {
              print("pars:")
              print(pars)
            }
          }
        }
      } else {
        stop("Pars does not have the correct length.")
      }
    }

  } else {

    if (!all(is.finite(y) || is.na(y) || (y_as_log && y == -Inf))) {
      if (verbose == 2) {
        print("y:")
        print(y)
      }
      stop("y must contain only real numbers, NA-values, and/or -Inf (if y_as_log is set to TRUE).")
    }

    if (min(y) < 0) {
      if (verbose) {
        warning("Warning: Negative y data.")
        if (verbose == 2) {
          print("y:")
          print(y)
        }
      }
    }

    if (max(y) > (1 + 99 * y_as_pct)) {
      if (verbose) {
        warning("Warning: y data exceeds negative control.")
        if (verbose == 2) {
          print("y:")
          print(y)
        }
      }
    }

    if (missing(pars)) {

      if (y_as_log == FALSE && min(y) < 0) {
        if (verbose) {
          warning("Negative y-values encountered. y data may be inappropriate, or 'y_as_log' flag may be set incorrectly.")
          if (verbose == 2) {
            print("y:")
            print(y)
            print("y_as_log:")
            print(y_as_log)
          }
        }
      }

      if (y_as_pct == TRUE && max(y) < 5) {
        if (verbose) {
          warning("Warning: 'y_as_pct' flag may be set incorrectly.")
          if (verbose == 2) {
            print("y:")
            print(y)
            print("y_as_pct:")
            print(y_as_pct)
          }
        }
      }

      if (y_as_pct == FALSE && max(y) > 5) {
        if (verbose) {
          warning("Warning: 'y_as_pct' flag may be set incorrectly.")
          if (verbose == 2) {
            print("y:")
            print(y)
            print("y_as_pct:")
            print(y_as_pct)
          }
        }
      }

      if (!missing(x) && length(x) != length(y)) {
        if (verbose == 2) {
          print("x:")
          print(x)
          print("y:")
          print(y)
        }
        stop("Vector of x-values is not of same length as vector of y-values.")
      }

    } else {
      stop("Please pass in only one of 'pars' and 'y', as it is unclear which to use in the computation.")
    }
  }
  
  if (!missing(lower) && !missing(upper)) {
    if (!(is.double(lower))) {
      if (verbose == 2) {
        print("lower:")
        print(lower)
      }
      stop("The lower bound must be a positive real number.")
    }
    
    if (!(is.double(lower))) {
      if (verbose == 2) {
        print("upper:")
        print(upper)
      }
      stop("The upper bound must be a positive real number.")
    }
    
    if (lower >= upper) {
      if (verbose == 2) {
        print("lower:")
        print(lower)
        print("upper:")
        print(upper)
      }
      stop("The lower bound of the range of allowed x-values must be less than the upper bound.")
    }
    
    if (lower < 0) {
      if (verbose == 2) {
        print("lower:")
        print(lower)
      }
      stop("The lower bound of the range of allowed x-values must be nonnegative.")
    }
    
    if (upper < 0) {
      if (verbose == 2) {
        print("upper:")
        print(upper)
      }
      stop("The upper bound of the range of allowed x-values must be nonnegative.")
    }
  }
}
