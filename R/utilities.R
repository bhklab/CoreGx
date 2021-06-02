## Utility function for the Gx suite packages ------------------------------ 
## FIXME:: Put these functions in sections based on similar purposes

# ExpressionSet to SummarizedExperiment -----------------------------------

#' CSet molecularProfiles from ESets to SEs
#'
#' Converts all ExpressionSet objects within the molecularProfiles slot of a 
#'   CoreSet to SummarizedExperiments
#'
#' @param cSet \code{S4} A CoreSet containing molecular data in ExpressionSets
#'
#' @return \code{S4} A CoreSet containing molecular data in a 
#'   SummarizedExperiments
#' 
#' @importFrom BiocParallel bplapply
#' @importFrom SummarizedExperiment SummarizedExperiment Assays assay 
#'   assayNames assayNames<-
#' @importFrom Biobase exprs fData pData annotation protocolData 
#'   assayDataElementNames
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom stats setNames
#' @keywords internal
.convertCSetMolecularProfilesToSE <- function(cSet) {

    eSets <- molecularProfilesSlot(cSet)  # Extract eSet data
    
    molecularProfilesSlot(cSet) <- lapply(eSets, function(eSet) {
        
        # Change rownames from probes to EnsemblGeneId for rna data type
        if (grepl("^rna$", Biobase::annotation(eSet))) {
            rownames(eSet) <- Biobase::fData(eSet)$EnsemblGeneId
        }
        
        # Build summarized experiment from eSet TODO:: Do we want to pass an environment for better memory efficiency?
        SE <- SummarizedExperiment::SummarizedExperiment(assays = SimpleList(as.list(Biobase::assayData(eSet))), rowData = S4Vectors::DataFrame(Biobase::fData(eSet), 
            rownames = rownames(Biobase::fData(eSet))), colData = S4Vectors::DataFrame(Biobase::pData(eSet), rownames = rownames(Biobase::pData(eSet))), 
            metadata = list(experimentData = eSet@experimentData, annotation = Biobase::annotation(eSet), protocolData = Biobase::protocolData(eSet)))
        ## TODO:: Determine if this can be done in the SE constructor?  Extract names from expression set
        assayNames(SE) <- assayDataElementNames(eSet)
        # Assign SE to cSet
        mDataType <- Biobase::annotation(eSet)
        molecularProfilesSlot(cSet)[[mDataType]] <- SE
    })
    setNames(cSet@molecularProfiles, names(eSets))
    cSet
}

# sanityCheck -------------------------------------------------------------

## TODO:: Add documentation!
#' @export
#' @noRd
.sanitizeInput <- function(x, y, lower, upper, pars, x_as_log, y_as_log, 
    y_as_pct, trunc, verbose = FALSE) 
{
    # Set to 2 to see debug printouts
    
    if (!is.logical(x_as_log)) {
        if (verbose == 2) {
            message("x_as_log:")
            message(x_as_log)
        }
        stop("'x_as_log' is not a logical.")
    }
    
    if (!is.logical(y_as_log)) {
        if (verbose == 2) {
            message("y_as_log:")
            message(y_as_log)
        }
        stop("'y_as_log' is not a logical.")
    }
    
    if (!is.logical(y_as_pct)) {
        if (verbose == 2) {
            message("y_as_pct:")
            message(y_as_pct)
        }
        stop("'y_as_pct' is not a logical.")
    }
    
    if (!is.logical(trunc)) {
        if (verbose == 2) {
            message("trunc:")
            message(trunc)
        }
        stop("'trunc' is not a logical.")
    }
    
    if (y_as_pct && y_as_log) {
        if (verbose == 2) {
            message("y_as_pct:")
            message(y_as_pct)
            message("y_as_log:")
            message(y_as_log)
        }
        warning("y_as_pct and y_as_log flags should almost certainly not both be TRUE.")
    }
    
    if (!(verbose %in% c(0, 1, 2))) {
        message("verbose:")  #can't have the if(verbose == 2) statement here since verbose itself is the problem!
        message(verbose)
        stop("'verbose' flag is not set correctly.")
    }
    
    if (!missing(x)) {
        if (!all(is.finite(x) | is.na(x)) || (x_as_log && any(x == -Inf))) {
            if (verbose == 2) {
                message("x:")
                message(x)
            }
            stop("x must contain only real numbers, NA-values, and/or -Inf (if x_as_log flag is set to TRUE).")
        }
        
        if (x_as_log == FALSE && min(x) < 0) {
            if (verbose == 2) {
                message("x:")
                message(x)
                message("x_as_log:")
                message(x_as_log)
            }
            stop("Negative x-values encountered. Data may be inappropriate, or 'x_as_log' flag may be set incorrectly.")
        }
        if (length(unique(x)) < 3) {
            stop("Please pass in at least 3 unique dose points.")
        }
    }
    
    if (missing(y)) {
        if (missing(pars)) {
            stop("Both 'pars' and 'y' missing, please pass in some data!")
        } else {
            
            if (pars[[1]] < 0 || pars[[2]] < 0) {
                # HS or alpha
                if (verbose == 2) {
                  message("pars:")
                  message(pars)
                }
                warning("Curve parameters may be inappropriately set to negative values.")
            }
            
            if (length(pars) == 3) {
                # and thus we are in PharmacoGx
                if (x_as_log == FALSE && pars[[3]] < 0) {
                  message("pars:")
                  message(pars[[3]])
                  message("x_as_log:")
                  message(x_as_log)
                  stop("'x_as_log' flag may be set incorrectly, as the EC50 is negative when a positive value is expected.")
                }
                
                if (y_as_pct == FALSE) {
                  if (pars[[2]] > 1) {
                    if (verbose == 2) {
                      message("pars:")
                      message(pars[[2]])
                      message("y_as_pct:")
                      message(y_as_pct)
                    }
                    warning("Warning: 'y_as_pct' flag may be set incorrectly.")
                  }
                }
            } else if (length(pars) == 2) {
                if (pars[[1]] < pars[[2]]) {
                  if (verbose) {
                    warning("Alpha is greater than beta.")
                    if (verbose == 2) {
                      message("pars:")
                      message(pars)
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
                message("y:")
                message(y)
            }
            stop("y must contain only real numbers, NA-values, and/or -Inf (if y_as_log is set to TRUE).")
        }
        
        if (min(y) < 0) {
            if (verbose) {
                warning("Warning: Negative y data.")
                if (verbose == 2) {
                  message("y:")
                  message(y)
                }
            }
        }
        
        if (max(y) > (1 + 99 * y_as_pct)) {
            if (verbose) {
                warning("Warning: y data exceeds negative control.")
                if (verbose == 2) {
                  message("y:")
                  message(y)
                }
            }
        }
        
        if (missing(pars)) {
            
            if (y_as_log == FALSE && min(y) < 0) {
                if (verbose) {
                  warning("Negative y-values encountered. y data may be inappropriate, or 'y_as_log' flag may be set incorrectly.")
                  if (verbose == 2) {
                    message("y:")
                    message(y)
                    message("y_as_log:")
                    message(y_as_log)
                  }
                }
            }
            
            if (y_as_pct == TRUE && max(y) < 5) {
                if (verbose) {
                  warning("Warning: 'y_as_pct' flag may be set incorrectly.")
                  if (verbose == 2) {
                    message("y:")
                    message(y)
                    message("y_as_pct:")
                    message(y_as_pct)
                  }
                }
            }
            
            if (y_as_pct == FALSE && max(y) > 5) {
                if (verbose) {
                  warning("Warning: 'y_as_pct' flag may be set incorrectly.")
                  if (verbose == 2) {
                    message("y:")
                    message(y)
                    message("y_as_pct:")
                    message(y_as_pct)
                  }
                }
            }
            
            if (!missing(x) && length(x) != length(y)) {
                if (verbose == 2) {
                  message("x:")
                  message(x)
                  message("y:")
                  message(y)
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
                message("lower:")
                message(lower)
            }
            stop("The lower bound must be a positive real number.")
        }
        
        if (!(is.double(lower))) {
            if (verbose == 2) {
                message("upper:")
                message(upper)
            }
            stop("The upper bound must be a positive real number.")
        }
        
        if (lower >= upper) {
            if (verbose == 2) {
                message("lower:")
                message(lower)
                message("upper:")
                message(upper)
            }
            stop("The lower bound of the range of allowed x-values must be less than the upper bound.")
        }
        
        if (lower < 0) {
            if (verbose == 2) {
                message("lower:")
                message(lower)
            }
            stop("The lower bound of the range of allowed x-values must be nonnegative.")
        }
        
        if (upper < 0) {
            if (verbose == 2) {
                message("upper:")
                message(upper)
            }
            stop("The upper bound of the range of allowed x-values must be nonnegative.")
        }
    }
}


# getSupportVec -----------------------------------------------------------

## get vector of interpolated concentrations for graphing purposes
#' .getSupportVec 
#'
#' @param x An input vector of dosages
#' @param output_length The length of the returned support vector
#' 
#' @return \code{numeric} A numeric vector of interpolated concentrations
#' 
#' @export
#' @noRd
.getSupportVec <- function(x, output_length = 1001) {
    return(seq(from = min(x), to = max(x), length.out = output_length))
}

#### reformatData ------------------------------------------------------------

#' @export
#' @noRd
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
        if (any(is.na(x) & (!is.na(y)))) {
            warning("Missing x-values with non-missing y-values encountered. Removed y-values correspoding to those x-values.")
            myx <- !is.na(x)
            x <- as.numeric(x[myx])
            y <- as.numeric(y[myx])
        }
        
        if (any((!is.na(x)) & is.na(y))) {
            warning("Missing y-values with non-missing x-values encountered. Removed x values correspoding to those y-values.")
            myy <- !is.na(y)
            x <- as.numeric(x[myy])
            y <- as.numeric(y[myy])
        }
        
        if (is.unsorted(x)) {
            y <- y[xOrder]
        }
        
        if (trunc) {
            y = pmin(as.numeric(y), 1)
            y = pmax(as.numeric(y), 0)
        }
        
        if (x_to_log) {
            x0s <- which(x == -Inf)
            if (length(x0s) > 0) {
                x <- x[-x0s]
                y <- y[-x0s]
            }
        }
        
        if (y_to_log) {
            if (any(y <= 0)) {
                warning("Transforming y to log with non-positive y values present, therefore removing.")
                x <- x[y > 0]
                y <- y[y > 0]
                if (!length(x)) {
                  stop("No valid positive y values encountered, please pass in some data.")
                }
            }
            y <- log(y)
        }
        
        if (y_to_frac) {
            y <- y/100
        }
        
        if (length(unique(x)) < 3) {
            stop("Less than 3 unique dose points left after cleaning data, please pass in enough valid measurements.")
            
        }
        
        return(list(x = x, y = y))
    }
    
    if (!missing(pars)) {
        
        if (x_to_log && length(pars) == 3) {
            pars[[3]] <- log10(pars[[3]])
        }
        
        if (y_to_frac && length(pars) == 3) {
            pars[[2]] <- pars[[2]]/100
        }
        
        return(list(x = x, pars = pars))
    }
}




# multinom ----------------------------------------------------------------

#' @export
#' @noRd
.multinom <- function(x, y) {
    coeff <- 1
    for (i in seq_len(length(y))) {
        coeff <- coeff * choose(x, y[i])
        x <- x - y[i]
    }
    return(coeff)
}

# medncauchys -------------------------------------------------------------

## TODO:: Add documentation to these functions
#' @importFrom stats rcauchy
#' @export
#' @keywords internal
#' @noRd
.rmedncauchys = function(N, n, scale) {
    x <- matrix(NA, nrow = 1, ncol = N)
    for (i in seq_len(N)) {
        x[i] <- median(rcauchy(n, scale = scale))
    }
    return(x)
}

#' @importFrom stats dcauchy pcauchy integrate
#' @export
#' @keywords internal
#' @noRd
.dmedncauchys = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- matrix(NA, nrow = 1, ncol = length(x))
    for (g in seq_along(x)) {
        if (n[g]%%2 == 0) {
            y[g] <- 2 * .multinom(n[g], c(n[g]/2 - 1, n[g]/2 - 1)) * tryCatch(integrate(f = function(j) {
                (pcauchy(x[g] - j/2, scale = scale[g]))^(n[g]/2 - 1) * (1 - pcauchy(x[g] + j/2, scale = scale[g]))^(n[g]/2 - 1) * dcauchy(x[g] - 
                  j/2, scale = scale[g]) * dcauchy(x[g] + j/2, scale = scale[g])
            }, lower = 0, upper = Inf, subdivisions = divisions)[[1]], error = function(e) {
                if (divisions == 1) {
                  wseq <- c(1, 4, 1)
                } else {
                  wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
                }
                aseq <- seq(from = 0, to = pi/2, length.out = 2 * divisions + 1)
                tseq <- tan(aseq)/2
                return(sum((pcauchy(x[g] + tseq, scale = scale[g]))^(n[g]/2 - 1) * (pcauchy(x[g] - tseq, scale = scale[g]))^(n[g]/2 - 1) * 
                  dcauchy(x[g] + tseq, scale = scale[g]) * dcauchy(x[g] - tseq, scale = scale[g])/(cos(aseq))^2 * wseq) * (aseq[2] - aseq[1])/6)
            })
        } else {
            y[g] <- .multinom(n[g], c((n[g] - 1)/2, (n[g] - 1)/2)) * (pcauchy(x[g], scale = scale[g]))^((n[g] - 1)/2) * (1 - pcauchy(x[g], 
                scale = scale[g]))^((n[g] - 1)/2) * dcauchy(x[g], scale = scale[g])
        }
    }
    return(y)
}

#' @importFrom stats pcauchy integrate
#' @export
#' @keywords internal
#' @noRd
.pmedncauchys = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- integer(length(x))
    for (g in seq_along(x)) {
        if (n[g]%%2 == 0) {
            y[g] <- tryCatch(integrate(f = function(k) {
                .dmedncauchys(k, n[g], scale[g])
            }, lower = -Inf, upper = x[g], subdivisions = divisions)[[1]], error = function(e) {
                wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
                aseq <- seq(from = -pi/2, to = atan(x[g]), length.out = 2 * divisions + 1)
                return(sum(.dmedncauchys(tan(aseq), n[g], scale[g]) * wseq/(cos(aseq))^2) * (aseq[3] - aseq[1])/6)
            })
        } else {
            y[g] <- 0
            Fx <- pcauchy(x[g], scale = scale[g])
            for (i in 0:((n[g] - 1)/2)) {
                y[g] <- y[g] + choose((n[g] - 1)/2, i) * (-1)^i * Fx^((n[g] + 1)/2 + i)/((n[g] + 1)/2 + i)
            }
            y[g] <- y[g] * .multinom(n[g], c((n[g] - 1)/2, (n[g] - 1)/2))
        }
    }
    return(y)
}

#' @importFrom stats integrate
#' @keywords internal
#' @export
#' @noRd
.edmedncauchys = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- numeric(length(x))
    for (g in seq_along(y)) {
        if (x[g] > 0) {
            upper <- Inf
            lower <- x[g]
        } else {
            upper <- x[g]
            lower <- -Inf
        }
        y[g] <- tryCatch(integrate(f = function(k) {
            (.dmedncauchys(k, n[g], scale[g]))^2
        }, lower = lower, upper = upper, subdivisions = divisions)[[1]], error = function(e) {
            wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
            aseq <- seq(from = atan(lower), to = atan(upper), length.out = 2 * divisions + 1)
            return(sum((.dmedncauchys(tan(aseq), n[g], scale[g]))^2 * wseq/(cos(aseq))^2) * (aseq[3] - aseq[1])/6)
        })
    }
    return(y)
}

#### mednnormals -------------------------------------------------------------

#' @export
#' @keywords internal
#' @noRd
.rmednnormals = function(N, n, scale) {
    x <- matrix(NA, nrow = 1, ncol = N)
    for (i in seq_len(N)) {
        x[i] <- median(rnorm(n, sd = scale))
    }
    return(x)
}

#' @importFrom stats rnorm  dnorm
#' @export
#' @keywords internal
#' @noRd
.dmednnormals = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- matrix(NA, nrow = 1, ncol = length(x))
    for (g in seq_along(x)) {
        if (n[g]%%2 == 0) {
            y[g] <- 2 * .multinom(n[g], c(n[g]/2 - 1, n[g]/2 - 1)) * tryCatch(integrate(f = function(j) {
                (pnorm(x[g] - j/2, sd = scale[g]))^(n[g]/2 - 1) * (1 - pnorm(x[g] + j/2, sd = scale[g]))^(n[g]/2 - 1) * dnorm(x[g] - j/2, 
                  sd = scale[g]) * dnorm(x[g] + j/2, sd = scale[g])
            }, lower = 0, upper = Inf, subdivisions = divisions)[[1]], error = function(e) {
                if (divisions == 1) {
                  wseq <- c(1, 4, 1)
                } else {
                  wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
                }
                aseq <- seq(from = 0, to = pi/2, length.out = 2 * divisions + 1)
                tseq <- tan(aseq)/2
                return(sum((pnorm(x[g] + tseq, sd = scale[g]))^(n[g]/2 - 1) * (pnorm(x[g] - tseq, sd = scale[g]))^(n[g]/2 - 1) * dnorm(x[g] + 
                  tseq, sd = scale[g]) * dnorm(x[g] - tseq, sd = scale[g])/(cos(aseq))^2 * wseq) * (aseq[2] - aseq[1])/6)
            })
        } else {
            y[g] <- .multinom(n[g], c((n[g] - 1)/2, (n[g] - 1)/2)) * (pnorm(x[g], sd = scale[g]))^((n[g] - 1)/2) * (1 - pnorm(x[g], sd = scale[g]))^((n[g] - 
                1)/2) * dnorm(x[g], sd = scale[g])
        }
    }
    return(y)
}

#' @importFrom stats integrate
#' @export
#' @keywords internal
#' @noRd
.pmednnormals = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- numeric(length(x))
    for (g in seq_along(x)) {
        if (n[g]%%2 == 0) {
            y[g] <- tryCatch(integrate(f = function(k) {
                .dmednnormals(k, n[g], scale[g])
            }, lower = -Inf, upper = x[g], subdivisions = divisions)[[1]], error = function(e) {
                wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
                aseq <- seq(from = -pi/2, to = atan(x[g]), length.out = 2 * divisions + 1)
                return(sum(.dmednnormals(tan(aseq), n[g], scale[g]) * wseq/(cos(aseq))^2) * (aseq[3] - aseq[1])/6)
            })
        } else {
            y[g] <- 0
            Fx <- pnorm(x[g], sd = scale[g])
            for (i in 0:((n[g] - 1)/2)) {
                y[g] <- y[g] + choose((n[g] - 1)/2, i) * (-1)^i * Fx^((n[g] + 1)/2 + i)/((n[g] + 1)/2 + i)
            }
            y[g] <- y[g] * .multinom(n[g], c((n[g] - 1)/2, (n[g] - 1)/2))
        }
    }
    return(y)
}

#' @importFrom stats integrate
#' @export
#' @keywords intenral
#' @noRd
.edmednnormals = function(x, n, scale, divisions = 100) {
    n <- rep(n, times = length(x)/length(n))
    scale <- rep(scale, times = length(x)/length(scale))
    y <- numeric(length(x))
    for (g in seq_along(y)) {
        if (x[g] > 0) {
            upper <- Inf
            lower <- x[g]
        } else {
            upper <- x[g]
            lower <- -Inf
        }
        y[g] <- tryCatch(integrate(f = function(k) {
            (.dmednnormals(k, n[g], scale[g]))^2
        }, lower = lower, upper = upper, subdivisions = divisions)[[1]], error = function(e) {
            wseq <- c(1, 4, rep(c(2, 4), times = divisions - 1), 1)
            aseq <- seq(from = atan(lower), to = atan(upper), length.out = 2 * divisions + 1)
            return(sum((.dmednnormals(tan(aseq), n[g], scale[g]))^2 * wseq/(cos(aseq))^2) * (aseq[3] - aseq[1])/6)
        })
    }
    return(y)
}


# fitCurve ----------------------------------------------------------------

## TODO:: Add function documentation
#' .fitCurve
#' 
#' Curve optimization from 1 variable to 1 variable, using L-BFSG-B from optim, 
#' with fallback to pattern search if optimization fails to converge. 
#' 
#' @param x `numeric` input/x values for function
#' @param y `numeric` output/y values for function
#' @param f `function` function f, parameterized by parameters to optimize 
#' @param density `numeric` how many points in the dimension of each parameter should 
#'   be evaluated (density of the grid)
#' @param step initial step size for pattern search.
#' @param precision `numeric` smallest step size used in pattern search, once step size drops below this value,
#'   the search terminates.  
#' @param lower_bounds `numeric` lower bounds for the paramater search space
#' @param upper_bounds `numeric` upper bounds for the parameter search space
#' @param median_n `integer` number of technical replicates per measured point in x. Used to 
#'   evaluate the proper median distribution for the normal and cauchy error models
#' @param scale `numeric` scale on which to measure probability for the error model (roughly SD of error)
#' @param family `character` which error family to use. Currently, `normal` and `cauchy` are implemented
#' @param trunc `logical` Whether or not to truncate the values at 100% (1.0)
#' @param verbose `logical` should diagnostic messages be printed?
#' @param gritty_guess `numeric` intitial, uninformed guess on parameter values (usually heuristic)
#' @param span ['numeric'] can be safely kept at 1, multiplicative ratio for initial step size in pattern search. 
#'   Must be larger than precision. 
#' @importFrom stats optim var
#' @export
#' @keywords internal
#' @noRd
.fitCurve <- function(x, y, f, density, step, precision, lower_bounds, upper_bounds, scale, family, median_n, trunc, verbose, gritty_guess, 
    span = 1) {
    
    guess <- tryCatch(optim(par = gritty_guess, fn = function(t) {
        .residual(x = x, y = y, n = median_n, pars = t, f = f, scale = scale, family = family, trunc = trunc)
    }, lower = lower_bounds, upper = upper_bounds, control = list(factr = 1e-08, trace = 0), method = "L-BFGS-B"), error = function(e) {
        list(par = gritty_guess, convergence = -1)
    })
    failed = guess[["convergence"]] != 0
    guess <- guess[["par"]]
    
    guess_residual <- .residual(x = x, y = y, n = median_n, pars = guess, f = f, scale = scale, family = family, trunc = trunc)
    gritty_guess_residual <- .residual(x = x, y = y, n = median_n, pars = gritty_guess, f = f, scale = scale, family = family, trunc = trunc)
    
    if (failed || any(is.na(guess)) || guess_residual >= gritty_guess_residual) {
        guess <- .meshEval(x = x, y = y, f = f, guess = gritty_guess, lower_bounds = lower_bounds, upper_bounds = upper_bounds, density = density, 
            n = median_n, scale = scale, family = family, trunc = trunc)
        guess_residual <- .residual(x = x, y = y, n = median_n, pars = guess, f = f, scale = scale, family = family, trunc = trunc)
        
        guess <- .patternSearch(x = x, y = y, f = f, guess = guess, n = median_n, guess_residual = guess_residual, lower_bounds = lower_bounds, 
            upper_bounds = upper_bounds, span = span, precision = precision, step = step, scale = scale, family = family, trunc = trunc)
    }
    
    y_hat <- do.call(f, list(x, guess))
    
    Rsqr <- 1 - var(y - y_hat)/var(y)
    attr(guess, "Rsquare") <- Rsqr
    
    return(guess)
}

# meshEval ----------------------------------------------------------------
#' meshEval
#'
#' generate an initial guess for dose-response curve parameters by evaluating
#' the residuals at different lattice points of the search space
#'
#' @export
#' @keywords internal
#' @noRd
# ##FIXME:: Why is this different in PharmacoGx?
.meshEval <- function(x, y, f, guess, lower_bounds, upper_bounds, density, n, scale, family, trunc) {
    pars <- NULL
    guess_residual <- .residual(x = x, y = y, n = n, pars = guess, f = f, scale = scale, family = family, trunc = trunc)
    
    periods <- matrix(NA, nrow = length(guess), ncol = 1)
    names(periods) <- names(guess)
    periods[1] <- 1
    
    if (length(guess) > 1) {
        for (par in 2:length(guess)) {
            periods[par] <- periods[par - 1] * density[par] * (upper_bounds[par] - lower_bounds[par])
        }
    }
    
    currentPars <- lower_bounds
    for (point in seq_len(prod((upper_bounds - lower_bounds) * density))) {
        for (par in seq_along(guess)) {
            if (point%%periods[par] == 0) {
                if (currentPars[par] >= upper_bounds[par]) {
                  currentPars[par] <- lower_bounds[par]
                } else {
                  currentPars[par] <- currentPars[par] + 1/density[par]
                }
            }
        }
        test_guess_residual <- .residual(x = x, y = y, n = n, pars = currentPars, f = f, scale = scale, family = family, trunc = trunc)
        if (!length(test_guess_residual) || (!is.finite(test_guess_residual) && test_guess_residual != Inf)) {
            stop(paste0(" Test Guess Residual is: ", test_guess_residual, "\n", "Other Pars:\n", "x: ", paste(x, collapse = ", "), "\n", 
                "y: ", paste(y, collapse = ", "), "\n", "n: ", n, "\n", "pars: ", pars, "\n", "scale: ", scale, "\n", "family : ", family, 
                "\n", "Trunc ", trunc))
        }
        if (test_guess_residual < guess_residual) {
            guess <- currentPars
            guess_residual <- test_guess_residual
        }
    }
    
    return(guess)
}


# Set Operations ----------------------------------------------------------

## TODO:: Can we implement this as an extension of the BiocGenerics::setdiff?
#' Utility to find the symmetric set difference of a list of two or more 
#' vectors or lists
#' 
#' The function finds the symmetric set differnces between all the arguments, 
#' defined as Union(args)-Intersection(args)
#' 
#' @examples 
#' list1 <- list('a', 'b', 'c')
#' list2 <- list('a', 'c')
#' list3 <- list('a', 'c', 'd')
#' listAll <- .symSetDiffList(list1, list2, list3)
#' listAll
#' 
#' @param ... A list of or any number of vector like objects of the same mode,
#'   which could also be operated on by the native R set operations
#'
#' @return A vector like object of the same mode as the first argument,
#'   containing only the symmetric set difference
#'
#' @export
#' @keywords internal
.symSetDiffList <- function(...) {
    return(setdiff(.unionList(...), .intersectList(...)))
}

## FIXME:: This should be implemented as an extension of the intersect generic provided in the BiocGenerics package!
#' Intersect A List of More Than Two Vectors
#'
#' Utility to find the intersection between a list of more than two vectors or
#'   lists This function extends the native intersect function to work on two 
#'   or more arguments.
#' 
#' @examples 
#' list1 <- list('a', 'b', 'c')
#' list2 <- list('a', 'c')
#' list3 <- list('a', 'c', 'd')
#' listAll <- .intersectList(list1, list2, list3)
#' listAll
#' 
#' @param ... A list of or any number of vector like objects of the same mode,
#'   which could also be operated on by the native R set operations
#'   
#' @return A vector like object of the same mode as the first argument,
#'   containing only the intersection common to all arguments to the function
#'   
#' @export
#' @keywords internal
.intersectList <- function(...) {
    args <- list(...)
    nargs <- length(args)
    if (nargs == 0) {
        return(args)
    }
    if (nargs == 1) {
        if (nargs == 1 && is.list(args[[1]])) {
            do.call(".intersectList", args[[1]])
        } else {
            return(args[[1]])
        }
    } else if (nargs == 2) {
        return(intersect(args[[1]], args[[2]]))
    } else {
        return(intersect(args[[1]], .intersectList(args[-1])))
    }
}


## FIXME:: This should be implemented as an extension of the union generic from BiocGenerics
#' Utility to find the union between a list of more than two vectors or
#' lists
#' 
#' This function extends the native union function to work on two or more
#' arguments.
#' 
#' @examples 
#' list1 <- list('a', 'b')
#' list2 <- list('a', 'c')
#' list3 <- list('c', 'd')
#' listAll <- .unionList(list1, list2, list3)
#' listAll
#' 
#' @param ... A list of or any number of vector like objects of the same mode,
#'   which could also be operated on by the native R set operations
#'   
#' @return A vector like object of the same mode as the first argument,
#'   containing all the elements of all arguments passed to the function
#'   
#' @export
#' @keywords internal
.unionList <- function(...) {
    args <- list(...)
    nargs <- length(args)
    return(unique(unlist(do.call(c, args))))
}

#' @export
#' @keywords internal
#' @noRd
# @param matInd array indices @param dimsizes array containing size of array of interest in each dimension
.linInd <- function(matInd, dimsizes) {
    y <- matInd[1]
    if (length(dimsizes) > 1) {
        for (i in seq(2, length(dimsizes))) {
            y <- y + (matInd[i] - 1) * prod(dimsizes[seq_len(i - 1)])
        }
    }
    return(y)
}

#' @export
#' @keywords internal
#' @noRd
# @param linInd linear index @param dimsizes array containing size of array of interest in each dimension
.matInd <- function(linInd, dimsizes) {
    y <- matrix(0, nrow = length(dimsizes), ncol = 1)
    if (NROW(y) > 1) {
        for (i in seq(2, length(dimsizes))) {
            y[i] <- ceiling(linInd/prod(dimsizes[seq_len(i - 1)]))
            linInd <- linInd - (y[i] - 1) * prod(dimsizes[seq_len(i - 1)])
        }
    }
    y[1] <- linInd
    return(y)
}

#' @export
#' @keywords internal
#' @noRd
.patternSearch <- function(x, y, f, guess, n, guess_residual, lower_bounds, upper_bounds, span, precision, step, scale, family, trunc) {
    neighbours <- matrix(nrow = 2 * length(guess), ncol = length(guess))
    neighbour_residuals <- matrix(NA, nrow = 1, ncol = nrow(neighbours))
    
    while (span > precision) {
        for (neighbour in seq_len(nrow(neighbours))) {
            neighbours[neighbour, ] <- guess
            dimension <- ceiling(neighbour/2)
            if (neighbour%%2 == 1) {
                neighbours[neighbour, dimension] <- pmin(guess[dimension] + span * step[dimension], upper_bounds[dimension])
            } else {
                neighbours[neighbour, dimension] <- pmax(guess[dimension] - span * step[dimension], lower_bounds[dimension])
            }
            
            neighbour_residuals[neighbour] <- .residual(x = x, y = y, f = f, pars = neighbours[neighbour, ], n = n, scale = scale, family = family, 
                trunc = trunc)
        }
        
        if (min(neighbour_residuals) < guess_residual) {
            guess <- neighbours[which.min(neighbour_residuals)[1], ]
            guess_residual <- min(neighbour_residuals)
        } else {
            span <- span/2
        }
    }
    
    return(guess)
}



# Not Used? ------------------------------------------------------------------

### TODO:: Determine type of objects intended for this function
#' Getter for attributes of an object
#'
#' @param pars The object for which attributes are to be returned
#' @return A named vector where index `Rsquare` contains the attributes of the object
#' @export
#' @keywords internal
#' @noRd
.examineGOF <- function(pars) {
    return(c(Rsquare = attr(pars, "Rsquare")))
}


# Different in PharmacoGx? ------------------------------------------------

## TODO:: Write documentation
## FIXME:: Why is this different from PharmacoGx?
#' @title Residual calculation
#'
#' @return A \code{numeric} containing the estimated residuals for the model
#'   fit
#'
#' @export
#' @keywords internal
#' @noRd
.residual <- function(x, y, n, pars, f, scale = 0.07, family = c("normal", "Cauchy"), trunc = FALSE) {
    family <- match.arg(family)
    diffs <- do.call(f, list(x, pars)) - y
    
    if (family != "Cauchy") {
        if (trunc == FALSE) {
            
            if (n == 1) {
                return(sum(diffs^2))
            }
            
            return(sum(-log(.dmednnormals(diffs, n, scale))))
        } else {
            down_truncated <- abs(y) >= 1
            up_truncated <- abs(y) <= 0
            return(sum(-log(.dmednnormals(diffs[!(down_truncated | up_truncated)], n, scale))) + sum(-log(.edmednnormals(-diffs[up_truncated | 
                down_truncated], n, scale))))
        }
    } else {
        if (trunc == FALSE) {
            return(sum(-log(.dmedncauchys(diffs, n, scale))))
        } else {
            down_truncated <- abs(y) >= 1
            up_truncated <- abs(y) <= 0
            return(sum(-log(.dmedncauchys(diffs[!(down_truncated | up_truncated)], n, scale))) + sum(-log(.edmedncauchys(-diffs[up_truncated | 
                down_truncated], n, scale))))
        }
    }
}

## FIXME:: This function already exists as base::trimws? Is there any reason we need to reimplement it?
#' @export
#' @keywords internal
#' @noRd
.stripWhiteSpace <- function(str, method = c("both", "head", "tail")) {
    method <- match.arg(method)
    str2 <- NULL
    if (length(str) == 1) {
        switch(method, both = {
            str2 <- gsub("^[ \t]+", "", str)
            str2 <- gsub("[ \t]+$", "", str2)
        }, head = {
            str2 <- gsub("^[ \t]+", "", str)
        }, tail = {
            str2 <- gsub("[ \t]+$", "", str)
        })
        return(str2)
    } else {
        str2 <- vapply(str, .stripWhiteSpace, method = method, FUN.VALUE = character(1))
        return(str2)
    }
}


# ==== LongTable

#' Convenience function for collapsing a character vector
#'
#' @examples
#' .collapse(c("Vector", "of", "words")
#'
#' @param ... `pairlist` One or more character vectors
#' @param collapse `character` Argument to collapse of paste0, default is ' '.
#'
#' @return `character` A single character vector.
#'
#' @keywords internal
#' @export
#' @noRd
.collapse <- function(..., collapse=' ')
    paste0(..., collapse=collapse)

#' Returns a colorized error message (magenta)
#'
#' @examples
#' cat(.errorMsg('This ', 'is ', 'an ', 'error ', 'message'))
#'
#' @param ... `pairlist` One or more strings or character vectors, also
#'   accepts any params to paste0.
#'
#' @return `character` Colorized string with results from paste0(...)
#'
#' @keywords internal
#' @export
#' @noRd
.errorMsg <- function(..., collapse=', ') magenta$bold(paste0(..., collapse=collapse))

#' Returns a colorized warning message (cyan)
#'
#' @examples
#' cat(.warnMsg('This ', 'is ', 'a ', 'warning ', 'message'))
#'
#' @param ... `pairlist` One or more strings or character vectors, also
#'   accepts any params to paste0.
#'
#' @return `character` Colorized string with results from paste0(...)
#'
#' @keywords internal
#' @export
#' @noRd
.warnMsg <- function(..., collapse=', ') cyan$bold(paste0(..., collapse=collapse))


#' Get the types of all items in a list
#'
#' @examples
#' list <- list(c(1,2,3), c('a','b','c'))
#' is.items(list, 'character')
#'
#' @param list A `list` to get the types from
#' @param ... `pairlist` Additional arguments to FUN
#' @param FUN `function` or `character` Either a function, or the name
#'   of a function which returns a single logical value. The default function
#'   uses `is`, specify the desired type in `...`. You can also use other
#'   type checking functions such as is.character, is.numeric, or is.data.frame.
#'
#' @return `logical` A vector indicating if the list item is the specified
#'   type.
#'
#' @export
is.items <- function(list, ..., FUN=is)
    vapply(list, FUN=FUN, FUN.VALUE=logical(1), ...)

#' @export
.length_unique <- function(x) length(unique(x))

#' @export
.list_unique <- function(x) list(unique(x))

#' @export
.all_equals <- function(x, y) all(x == y)


#' Return the name of the function and the name of the package that function
#'   is in when called within an R function.
#'
#' For providing context in user messages, warnings and errors
#'
#' @param n `integer` How far up the call stack to look for context. Defaults to
#'   2 since it is assumed this function will be used inside of `message`,
#'   `warning` or `stop`.
#'
#' @return A `character` vector with the name of the function
#'   `.getExecutionContext` was called from, as well as the package name,
#'   if applicable.
#'
#' @md
#' @keywords internal
#' @importFrom rlang trace_back
#' @importFrom utils packageName
#' @noRd
#' @aliases .context
.getExecutionContext <- function(n=2) {

    # name of function which called this function
    callStack <- rlang::trace_back()$calls
    context <- deparse(callStack[[length(callStack) - n]][1])

    # remove function arguments
    context <- gsub('\\(.*\\)', '', context)

    # deal with getting function names from inside an lapply statement
    ## TODO:: clean this up
    if (grepl('.*lapply.*', context)) {
        context <- deparse(callStack[[length(callStack) - (n + 1)]][3])
        context <- gsub('\\(.*\\)', '', context)
        # deal with S4 lapply calls (e.g., endoapply)
        if (grepl('.*match.fun.*', context)) {
            context <- deparse(callStack[[length(callStack) - (n + 5)]][3])
            context <- gsub('\\(.*\\)', '', context)
        }
    } else if (grepl('.*mapply.*', context)) {
        context <- deparse(callStack[[length(callStack) - (n + 1)]][1])
        context <- gsub('\\(.*\\)', '', context)
        if (grepl('.*match.fun.*', context)) {
            context <- deparse(callStack[[length(callStack) - (n + 5)]][1])
            context <- gsub('\\(.*\\)', '', context)
        }
    } else if (grepl('.*FUN.*', context)) {
        context <- deparse(callStack[[length(callStack) - (n + 2)]][3])
        context <- gsub('\\(.*\\)', '', context)
        # deal with S4 lapply calls (e.g., endoapply)
        if (grepl('.*match.fun.*', context)) {
            context <- tryCatch({ 
                deparse(callStack[[length(callStack) - (n + 6)]][3])
            }, error=function(e) 'context_failed')
            context <- gsub('\\(.*\\)', '', context)
        }
    }
    if (!grepl('::', context)) context <- paste0(packageName(), '::', context)

    return(paste0('\n[', context, '] '))
}
#' @noRd
.context <- .getExecutionContext


#'
#'
#'
#'
#' @md
#' @export
.S4MethodContext <- function(generic, ...) {
    dots <- as.list(...)
    formals <- selectMethod(generic, signature=dots)
    context <- paste0(
        formals@target@package[1], '::`', # what package is the method from
        formals@generic, ',',  # what is the name of the generic
        paste0(formals@target@.Data, collapse=','), '-method`') # what is the method signature
    return(context)
}