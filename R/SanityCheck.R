.sanitizeInput <- function(x,
                          y,
                          pars,
                          x_as_log = FALSE,
                          y_as_log = FALSE,
                          y_as_pct = TRUE,
                          trunc = TRUE,
                          verbose = TRUE) { # Set to 2 to see debug printouts


  if (is.logical(x_as_log) == FALSE) {
    print(x_as_log)
    stop("'x_as_log' is not a logical.")
  }

  if (is.logical(y_as_log) == FALSE) {
    print(y_as_log)
    stop("'y_as_log' is not a logical.")
  }

  if (is.logical(y_as_pct) == FALSE) {
    print(y_as_pct)
    stop("'y_as_pct' is not a logical.")
  }

  if (is.logical(trunc) == FALSE) {
    print(trunc)
    stop("'trunc' is not a logical.")
  }

  if(!is.finite(verbose)){
    stop("'verbose' should be a logical (or numerical) argument.")
  }

  if(!missing(y) && !missing(x) && missing(pars))
  {
    if (length(x) != length(y)) {
      if(verbose == 2){
        print(x)
        print(y)
      }
      stop("Log concentration vector is not of same length as vector of y-values.")
    }

    if(any(is.na(x) & (!is.na(y)))){
      warning("Missing x-values with non-missing y-values encountered. Removing y-values correspoding to those x-values")

      myx <- !is.na(x)
      x <- as.numeric(x[myx])
      y <- as.numeric(y[myx])

    }
    if(any((!is.na(x)) & is.na(y))){

      warning("Missing y-values with non-missing x-values encountered. Removing x values correspoding to those y-values")

      myx <- !is.na(y)
      x <- as.numeric(x[myx])
      y <- as.numeric(y[myx])

    }
    x <- as.numeric(x[!is.na(x)])
    y <- as.numeric(y[!is.na(y)])


    #CHECK THAT FUNCTION INPUTS ARE APPROPRIATE
    if (prod(is.finite(x)) != 1) {
      print(x)
      stop("concentration vector contains elements which are not real numbers.")
    }

    if (prod(is.finite(y)) != 1) {
      print(y)
      stop("Vector of y-values contains elements which are not real numbers.")
    }


    if (min(y) < 0) {
      if (verbose) {
        warning("Warning: Negative y data.")
      }
    }

    if (max(y) > (1 + 99 * y_as_pct)) {
      if (verbose) {
        warning("Warning: y data exceeds negative control.")
      }
    }

    if (x_as_log == FALSE && min(x) < 0) {
      if (verbose == 2) {
        print(x)
        print(x_as_log)
      }
      stop("Negative x-values encountered. x data may be inappropriate, or 'x_as_log' flag may be set incorrectly.")
    }

    if (y_as_log == FALSE && min(y) < 0) {
      if (verbose == 2) {
        print(y)
        print(y_as_log)
        warning("Negative y-values encountered. y data may be inappropriate, or 'y_as_log' flag may be set incorrectly.")
      }
    }

    if (y_as_pct == TRUE && max(y) < 5) {
      warning("Warning: 'y_as_pct' flag may be set incorrectly.")
      if (verbose == 2) {
        print(y)
        print(y_as_pct)
      }
    }

    if (y_as_pct == FALSE && max(y) > 5) {
      warning("Warning: 'y_as_pct' flag may be set incorrectly.")
      if (verbose == 2) {
        print(y)
        print(y_as_pct)
      }
    }

    if(is.unsorted(x)){
      warning("concentration Values were unsorted. Sorting concentration and ordering y in same order")
      myx <- order(x)
      x <- x[myx]
      y <- y[myx]
    }

    #CONVERT DOSE-RESPONSE DATA TO APPROPRIATE INTERNAL REPRESENTATION
    if (x_as_log == FALSE ) {
      ii <- which(x == 0)
      if(length(ii) > 0) {
        x <- x[-ii]
        y <- y[-ii]
      }
      log_x <- log10(x)
    } else {
      log_x <- x
    }

    if (y_as_pct == TRUE) {
      y <- y / 100
    }

    if (trunc) {
      y = pmin(as.numeric(y), 1)
      y = pmax(as.numeric(y), 0)
    }

    return(list("log_x"=log_x, "y"=y))
  }

  if(!missing(pars) && missing(y)){
    if(is.list(pars)){

      pars <- unlist(pars)
    }
    if (x_as_log == FALSE && pars[[3]] < 0) {
      print("EC50 passed in as:")
      print(pars[[3]])
      stop("'x_as_log' flag may be set incorrectly, as the EC50 is negative when positive value is expected.")
    }

    if (!missing(pars) && (pars[[1]] < 0 || pars[[2]] < 0)) { #HS or alpha
      print(pars)
      warning("Parameters may be inappropriately set to negative values.")
    }

    if (y_as_pct == FALSE) {
      if (!missing(pars) && length(pars) == 3 && pars[[2]] > 1 && verbose) {
        print("Einf passed in as:")
        print(pars[[2]])
        warning("Warning: 'y_as_pct' flag may be set incorrectly.")
      }
    }

    if (!missing(pars) && length(pars) == 2 && pars[[1]] < pars[[2]] && verbose) {
      print(pars)
      warning("Alpha is greater than beta.")
    }

    if (x_as_log == FALSE){
      if (length(pars) == 3) {
        pars[[3]] <- log10(pars[[3]])
      }
    }

    if (y_as_pct == TRUE && length(pars) == 3) {
      pars[[2]] <- pars[[2]] / 100
    }
    if(missing(x)){
      return(list("pars" = pars))

    } else {
      x <- as.numeric(x[!is.na(x)])

      if (prod(is.finite(x)) != 1) {
        print(x)
        stop("concentration vector contains elements which are not real numbers.")
      }

      if (x_as_log == FALSE && min(x) < 0) {
        print(x)
        print(x_as_log)
        stop("Negative concentrations encountered. concentration data may be inappropriate, or 'x_as_log' flag may be set incorrectly.")
      }

      if (x_as_log == FALSE ) {
        ii <- which(x == 0)
        if(length(ii) > 0) {
          x <- x[-ii]
        }
        log_x <- log10(x)
      } else {
        log_x <- x
      }

      if(is.unsorted(x)){
        myx <- order(x)
        x <- x[myx]
      }
      return(list("pars"=pars, "log_x" = log_x))
    }
  }

  if(!missing(pars) && !missing(y)){

    stop("Please pass in only one of 'pars' and 'y', as it is unclear which to use in the computation.")
  }

  if(missing(pars) && missing(y)){

    stop("Both 'pars' and 'y' missing, please pass in some data!")
  }
}
