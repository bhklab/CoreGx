.reformatData <- function(x, y, pars, x_to_log, y_to_log, y_to_frac, trunc) {
  if (!(is.logical(x_to_log))) {
    stop("x_to_log must be a logical.")
  }

  if (!(is.logical(y_to_log))) {
    stop("y_to_log must be a logical.")
  }

  if (!(is.logical(y_to_frac))) {
    stop("y_to_frac must be a logical.")
  }

  if (any(is.na(x) & (!is.na(y)))){
    warning("Missing x-values with non-missing y-values encountered. Removed y-values correspoding to those x-values.")
    myx <- !is.na(x)
    x <- as.numeric(x[myx])
    y <- as.numeric(y[myx])
  }

  if (any((!is.na(x)) & is.na(y))){
    warning("Missing y-values with non-missing x-values encountered. Removed x values correspoding to those y-values.")
    myy <- !is.na(y)
    x <- as.numeric(x[myy])
    y <- as.numeric(y[myy])
  }

  if (is.unsorted(x)){
    warning("x-values were unsorted. Sorting x-values and ordering y-values correspondingly.")
    myx <- order(x)
    x <- x[myx]
    y <- y[myx]
  }

  if (x_to_log) {
    ii <- which(x == 0)
    if(length(ii) > 0) {
      x <- x[-ii]
      y <- y[-ii]
    }
    log_x <- log10(x)
    if (length(pars) == 3) {
      pars[[3]] <- log10(pars[[3]])
    }
  }

  if (y_to_frac) {
    y <- y / 100
    if (length(pars) == 3) {
      pars[[2]] <- pars[[2]] / 100
    }
  }

  if (trunc) {
    y = pmin(as.numeric(y), 1)
    y = pmax(as.numeric(y), 0)
  }

  if (y_to_log) {
    log_y <- log10(y)
  }
  
  return(list("x" = x,
              "y" = y,
              "pars" = pars))
}
