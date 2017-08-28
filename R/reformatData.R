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
  
  if (x_to_log) {
    x <- log10(x)
  }
  ### Shouldnt we sort y in same order?????
  if (is.unsorted(x)) {
    warning("x-values passed in unsorted. Sorting x-values and corresponding y-values (if passed in).")
    xOrder <- order(x)
    x <- x[xOrder]
  }
  
  if (!missing(y)) {
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
      y <- y[xOrder]
    }
    
    if (trunc) {
      y = pmin(as.numeric(y), 1)
      y = pmax(as.numeric(y), 0)
    }
    
    if (x_to_log) {
      x0s <- which(x == -Inf)
      if(length(x0s) > 0) {
        x <- x[-x0s]
        y <- y[-x0s]
      }
    }
    
    if (y_to_log) {
      if(any(y < 0)){
        warning("Transforming y to log with negative y values present, therefore removing.")
        x <- x[y >= 0]
        y <- y[y >= 0]
      }
      y <- log(y)
    }
    
    if (y_to_frac) {
      y <- y / 100
    }
    
    return(list("x" = x, "y" = y))
  }
  
  if (!missing(pars)) {
    
    if (x_to_log && length(pars) == 3) {
      pars[[3]] <- log10(pars[[3]])
    }
    
    if (y_to_frac && length(pars) == 3) {
      pars[[2]] <- pars[[2]] / 100
    }
    
    return(list("x" = x, "pars" = pars))
  }
}
