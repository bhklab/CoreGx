#' Curve fitting via `stats::optim` L-BFGS-B with fall-back grid/pattern search
#'   if convergence is not achieved.
#'
#' @description
#'
#' @param par `numeric` Vector of intial guesses for the parameters. For each
#'    index `i` of `par`, par[i] must be within the range (`lower[i]`, `upper[i]`).
#'    If only a single `upper` or `lower` value is present, that range is used
#'    for all parameters in `par`.
#' @param x `numeric` Values to evaluate `fn` for.
#' @param y `numeric` Target output values to optimze `fn` against.
#' @param fn `function` A function to optimize. Any `fn` arguments passed via
#'   `...` will be treated as constant and removed from the optimization. It
#'   is assumed that the first argument is the x value to optimize over and
#'   any subsequent arguments are free parameters to be optimized. Transformed
#'   to be optim compatible via `make_optim_function` is the first arguement
#'   isn't already `par`.
#' @param loss `character(1)` or `function` Either the name of one of the bundled
#'   loss functions (see details) or a custom loss function to compute for
#'   the output of `fn` over `x`.
#' @param lower `numeric(1)`
#' @param upper `numeric(1)`
#' @param density `numeric` how many points in the dimension of each parameter
#'   should be evaluated (density of the grid)
#' @param step initial step size for pattern search.
#' @param precision `numeric` smallest step size used in pattern search, once
#'   step size drops below this value, the search terminates.
#' @param ... `pairlist` Fall through arguments to `fn`.
#' @param loss_args `list` Additional argument to the `loss` function.
#'   These get passed to losss via `do.call` analagously to using `...`.
#' @param optim_only `logical(1)` Should the fall back methods when optim fails
#'   be skipped? Default is `FALSE`.
#' @param control `list` List of control parameters to pass to `optim`. See
#'   `?optim` for details.
#'
#' @examples
#' \donttest{
#' # Four parameter hill curve equation
#' hillEqn <- function(x, Emin, Emax, EC50, lambda) {
#'     (Emin + Emax * (x / EC50)^lambda) / (1 + (x / EC50)^lambda)
#' }
#' # Make some dummy data
#' doses <- rev(1000 / (2^(1:20)))
#' lambda <- 1
#' Emin <- 1
#' Emax <- 0.1
#' EC50 <- median(doses)
#' response <- hillEqn(doses, Emin=Emin, lambda=lambda, Emax=Emax, EC50=EC50)
#' nresponse <- response + rnorm(length(response), sd=sd(response)*0.1) # add noise
#' # 3-parameter optimization
#' 3par <- .fitCurve2(
#'     par=c(Emax, EC50, lambda),
#'     x=doses,
#'     y=nresponse,
#'     fn=hillEqn,
#'     Emin=Emin, # set this as constant in the function being optimized (via ...)
#'     loss=.normal_loss,
#'     loss_args=list(trunc=FALSE, n=1, scale=0.07),
#'     upper=c(1, max(doses), 6),
#'     lower=c(0, min(doses), 0)
#' )
#' # 2-parameter optimization
#' 2par <- .fitCurve2(
#'     par=c(Emax, EC50),
#'     x=doses,
#'     y=nresponse,
#'     fn=hillEqn,
#'     Emin=Emin, # set this as constant in the function being optimized (via ...)
#'     lambda=1,
#'     loss=.normal_loss,
#'     loss_args=list(trunc=FALSE, n=1, scale=0.07),
#'     upper=c(1, max(doses)),
#'     lower=c(0, min(doses))
#' )
#' }
#'
#' @details
#'
#'
#' @seealso [`CoreGx:::.meshEval2`], [`CoreGx:::.patternSearch2`]
#'
#' @return `numeric` Vector of optimal parameters for `fn` fit against `y`
#'   on the values of `x`.
#'
#' @importFrom stats optim
#' @export
.fitCurve2 <- function(par, x, y, fn, loss, lower=-Inf, upper=Inf,
        precision=1e-4, density=c(2, 10, 5), step= 0.5 / density, ...,
        loss_args=list(), span=1, optim_only=FALSE,
        control=list(factr=1e-08, ndeps=rep(1e-4, times=length(par)), trace = 0)
        ) {
    stopifnot(is.function(fn))
    stopifnot(is.function(loss) || is.character(loss))
    stopifnot(c("par", "x", "y", "fn") %in% formalArgs(loss))
    stopifnot(
        is.null(names(loss_args)) || all(names(loss_args) %in% formalArgs(loss))
    )
    stopifnot(
        (length(par) == length(upper) && length(par) == length(lower)) ||
        (length(upper) == 1 && length(lower) == 1)
    )
    optim_fn <- if (is_optim_compatible(fn)) {
        fn
    } else {
        make_optim_function(fn, ...)
    }
    guess <- optim(
        par=par,
        fn=function(p)
            do.call(loss,
                args=c(
                    list(par=p, x=x, y=y, fn=optim_fn), # mandatory loss args
                    loss_args # additional args to loss
                )
            ),
        upper=upper,
        lower=lower,
        control=control,
        method="L-BFGS-B"
    )

    failed <- guess[["convergence"]] != 0
    if (failed) guess <- list(par=par, convergence=(-1))
    guess <- guess[["par"]]

    guess_residual <- do.call(loss,
        args=c(list(par=guess, x=x, y=y, fn=optim_fn), loss_args))
    gritty_guess_residual <- do.call(loss,
        args=c(list(par=par, x=x, y=y, fn=optim_fn), loss_args))

    if (
        (failed || any(is.na(guess)) || guess_residual >= gritty_guess_residual)
        && !optim_only
    ) {
        guess <- .meshEval2(density=density, par=guess, x=x, y=y, fn=optim_fn,
            lower=lower, upper=upper, ..., loss=loss,loss_args=loss_args)
        guess_residual <- do.call(loss,
            args=c(list(par=guess, x=x, y=y, fn=optim_fn), loss_args))

        guess <- .patternSearch2(span=span, precision=precision, step=step,
            par=guess, par_residual=guess_residual, x=x, y=y, fn=optim_fn,
            loss=loss, lower=lower, upper=upper, ..., loss_args=loss_args)
    }

    y_hat <- optim_fn(par=guess, x=x)

    Rsqr <- 1 - (var(y - y_hat) / var(y))
    attr(guess, "Rsquare") <- Rsqr

    return(guess)
}


#' Compute the loss using the expectation of the likelihood of the median
#'   for N samples from a probability density function.
#'
#' @param .pdf `function` Probability density function to use for computing loss.
#' @param .edf `function` Expected likelihood function for the median of `n`
#'   random samples from `.pdf`.
#' @inheritParams .fitCurve2
#' @param n `numeric(1)`
#' @param scale `numeric(1)`
#' @param trunc `logical(1)`
#'
#' @return `numeric(1)` Loss of `fn` on `x` relative to `y`.
#'
#' @keywords interal
#' @noRd
.sampling_loss <- function(.pdf, .edf, par, x, y, fn, ..., n=1, scale=0.07,
        trunc=FALSE) {
    diffs <- fn(par=par, x=x, ...) - y
    if (trunc == FALSE) {
        if (n == 1 && grepl("normals", deparse(substitute(.pdf))))
            return(sum(diffs^2))
        return(sum(-log(.pdf(diffs, n, scale))))
    } else {
        down_truncated <- abs(y) >= 1
        up_truncated <- abs(y) <= 0
        return(
            sum(-log(.pdf(diffs[!(down_truncated | up_truncated)], n, scale))) +
            sum(-log(.edf(-diffs[up_truncated | down_truncated], n, scale)))
        )
    }
}


#' See docs for `.sampling_loss`
#' @keywords interal
#' @noRd
.normal_loss <- function(par, x, y, fn, ..., n=1, scale=0.07,
        trunc=FALSE) {
    .sampling_loss(.pdf=.dmednnormals, .edf=.edmednnormals, par=par, x=x, y=y,
        fn=fn, ..., n=n, scale=scale, trunc=trunc)
}



#' See docs for `.sampling_loss`
#' @keywords internal
#' @noRd
.cauchy_loss <- function(par, x, y, fn, ..., n=1, scale=0.07,
        trunc=FALSE) {
    .sampling_loss(.pdf=.dmedncauchys, .edf=.edmedncauchys, par=par, x=x, y=y,
        fn=fn, ..., n=n, scale=scale, trunc=trunc)
}

#' @export
#' @keywords internal
#' @noRd
.meshEval2 <- function(density, par, x, y, fn,
        loss=.normal_loss, lower, upper, ..., loss_args=list()) {
    # input validation
    stopifnot(is.function(fn))
    stopifnot(is.function(loss) || is.character(loss))
    stopifnot(c("par", "x", "y", "fn") %in% formalArgs(loss))
    stopifnot(
        is.null(names(loss_args)) || all(names(loss_args) %in% formalArgs(loss))
    )
    # make function amenable to use via optim
    optim_fn <- if (is_optim_compatible(fn)) {
        fn
    } else {
        make_optim_function(fn, ...)
    }
    par_loss <- do.call(loss,
        args=c(list(par=par, x=x, y=y, fn=optim_fn), loss_args))
    periods <- matrix(NA, nrow = length(par), ncol = 1)
    names(periods) <- names(par)
    periods[1] <- 1
    if (length(par) > 1) {
        for (p in seq(2, length(par))) {
            ## the par-1 is because we want 1 increment of par variable once
            ##   all previous variables have their values tested once.
            periods[p] <- periods[p - 1] * (density[p - 1] *
                (upper[p - 1] - lower[p - 1]) + 1)
        }
    }
    currentPars <- lower
    ## The plus one is because we include endpoints.
    for (point in seq_len(prod((upper - lower) * density + 1))) {
        test_par_loss <- do.call(loss,
            c(list(par=currentPars, x=x, y=y, fn=optim_fn), loss_args))
        ## Check for something catastrophic going wrong
        if (
            !length(test_par_loss) ||
            (!is.finite(test_par_loss) && test_par_loss != Inf)
        ) {
            stop(paste0(" Test Guess Loss is: ", test_par_loss, "\n",
                "Other Pars:\n", "x: ", paste(x, collapse = ", "), "\n",
                "y: ", paste(y, collapse = ", "), "\n", "n: ", n, "\n",
                "pars: ", currentPars, "\n", "scale: ", scale, "\n",
                "family : ", family, "\n", "Trunc ", trunc))
        }
        ## save the guess if its an improvement
        if (test_par_loss < par_loss) {
            par <- currentPars
            par_loss <- test_par_loss
        }
        ## increment the variable(s) that should be incremented this loop
        for (p in seq_along(par)) {
            if (point %% periods[p] == 0) {
                currentPars[p] <- currentPars[p] + 1 / density[p]
                if (currentPars[p] > upper[p]) {
                    currentPars[p] <- lower[p]
                }
            }
        }
    }
    return(par)
}


#' @export
#' @keywords internal
#' @noRd
.patternSearch2 <- function(span, precision, step, par, par_residual, x, y,
        lower, upper, fn, loss, ..., loss_args=list()) {
    # input validation
    stopifnot(is.function(fn))
    stopifnot(is.function(loss) || is.character(loss))
    stopifnot(c("par", "x", "y", "fn") %in% formalArgs(loss))
    stopifnot(
        is.null(names(loss_args)) || all(names(loss_args) %in% formalArgs(loss))
    )
    # make function amenable to use via optim
    optim_fn <- if (is_optim_compatible(fn)) {
        fn
    } else {
        make_optim_function(fn, ...)
    }
    # setup matrix for searching the parameter space
    neighbours <- matrix(nrow = 2 * length(par), ncol = length(par))
    neighbour_loss <- matrix(NA, nrow = 1, ncol = nrow(neighbours))

    while (span > precision) {
        for (neighbour in seq_len(nrow(neighbours))) {
            neighbours[neighbour, ] <- par
            dimension <- ceiling(neighbour / 2)
            if (neighbour %% 2 == 1) {
                neighbours[neighbour, dimension] <- pmin(
                    par[dimension] + span * step[dimension],
                    upper[dimension]
                )
            } else {
                neighbours[neighbour, dimension] <- pmax(
                    par[dimension] - span * step[dimension],
                    lower[dimension]
                )
            }

            neighbour_loss[neighbour] <- do.call(loss, args=c(
                list(par=neighbours[neighbour, ], x=x, y=y, fn=optim_fn),
                    loss_args))
        }

        if (min(neighbour_loss) < par_residual) {
            par <- neighbours[which.min(neighbour_loss)[1], ]
            par_residual <- min(neighbour_loss)
        } else {
            span <- span / 2
        }
    }
    return(par)
}


# ======================
# ==== Deprecating =====

#' .fitCurve
#'
#' Curve optimization from 1 variable to 1 variable, using L-BFSG-B from optim,
#' with fallback to pattern search if optimization fails to converge.
#'
#' @param x `numeric` input/x values for function
#' @param y `numeric` output/y values for function
#' @param f `function` function f, parameterized by parameters to optimize
#' @param density `numeric` how many points in the dimension of each parameter
#'   should be evaluated (density of the grid)
#' @param step initial step size for pattern search.
#' @param precision `numeric` smallest step size used in pattern search, once
#'   step size drops below this value, the search terminates.
#' @param lower_bounds `numeric` lower bounds for the paramater search space
#' @param upper_bounds `numeric` upper bounds for the parameter search space
#' @param median_n `integer` number of technical replicates per measured point
#'   in x. Used to evaluate the proper median distribution for the normal and
#'   cauchy error models
#' @param scale `numeric` scale on which to measure probability for the error
#'   model (roughly SD of error)
#' @param family `character` which error family to use. Currently, "normal"
#'   and "Cauchy" are implemented
#' @param trunc `logical` Whether or not to truncate the values at 100% (1.0)
#' @param verbose `logical` should diagnostic messages be printed?
#' @param gritty_guess `numeric` intitial, uninformed guess on parameter
#'   values (usually heuristic)
#' @param span ['numeric'] can be safely kept at 1, multiplicative ratio for
#'   initial step size in pattern search. Must be larger than precision.
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom stats optim var
#' @export
.fitCurve <- function(x, y, f, density, step, precision, lower_bounds,
    upper_bounds, scale, family, median_n, trunc, verbose, gritty_guess,
    span = 1) {

    guess <- tryCatch(optim(par = gritty_guess, fn = function(t) {
        .residual(
            x = x, y = y, n = median_n, pars = t, f = f,
            scale = scale, family = family, trunc = trunc
        )
    }, lower = lower_bounds, upper = upper_bounds, control = list(
        factr = 1e-08,
        ndeps = rep(1e-4, times = length(gritty_guess)),
        trace = 0
    ), method = "L-BFGS-B"), error = function(e) {
        list(par = gritty_guess, convergence = -1)
    })

    failed <- guess[["convergence"]] != 0
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

    y_hat <- do.call(f, list(x=x, par=guess))

    Rsqr <- 1 - (var(y - y_hat) / var(y))
    attr(guess, "Rsquare") <- Rsqr

    return(guess)
}



# meshEval ----------------------------------------------------------------
#' meshEval
#'
#' Generate an initial guess for dose-response curve parameters by evaluating
#' the residuals at different lattice points of the search space
#'
#' @export
#' @keywords internal
#' @noRd
# ##FIXME:: Why is this different in PharmacoGx?
.meshEval <- function(x, y, f, guess, lower_bounds, upper_bounds, density, n,
        scale, family, trunc) {
    pars <- NULL
    guess_residual <- .residual(x = x, y = y, n = n, pars = guess, f = f,
        scale = scale, family = family, trunc = trunc)

    periods <- matrix(NA, nrow = length(guess), ncol = 1)
    names(periods) <- names(guess)
    periods[1] <- 1

    if (length(guess) > 1) {
        for (par in 2:length(guess)) {
            ## the par-1 is because we want 1 increment of par variable once all previous variables have their values tested once.
            periods[par] <- periods[par - 1] * (density[par - 1] * (upper_bounds[par - 1] - lower_bounds[par - 1]) + 1)
        }
    }

    currentPars <- lower_bounds

    ## The plus one is because we include endpoints.
    for (point in seq_len(prod((upper_bounds - lower_bounds) * density + 1))) {

        test_guess_residual <- .residual(x = x, y = y, n = n, pars = currentPars, f = f, scale = scale, family = family, trunc = trunc)

        ## Check for something catastrophic going wrong
        if (!length(test_guess_residual) || (!is.finite(test_guess_residual) && test_guess_residual != Inf)) {
            stop(paste0(" Test Guess Residual is: ", test_guess_residual, "\n", "Other Pars:\n", "x: ", paste(x, collapse = ", "), "\n",
                "y: ", paste(y, collapse = ", "), "\n", "n: ", n, "\n", "pars: ", pars, "\n", "scale: ", scale, "\n", "family : ", family,
                "\n", "Trunc ", trunc))
        }
        ## save the guess if its an improvement
        if (test_guess_residual < guess_residual) {
            guess <- currentPars
            guess_residual <- test_guess_residual
        }
        ## increment the variable(s) that should be incremented this loop
        for (par in seq_along(guess)) {
            if (point%%periods[par] == 0) {
                currentPars[par] <- currentPars[par] + 1/density[par]

                if (currentPars[par] > upper_bounds[par]) {
                    currentPars[par] <- lower_bounds[par]
                }
            }
        }
    }

    return(guess)
}

#' @export
#' @keywords internal
#' @noRd
.patternSearch <- function(x, y, f, guess, n, guess_residual, lower_bounds,
        upper_bounds, span, precision, step, scale, family, trunc) {
    neighbours <- matrix(nrow = 2 * length(guess), ncol = length(guess))
    neighbour_residuals <- matrix(NA, nrow = 1, ncol = nrow(neighbours))

    while (span > precision) {
        for (neighbour in seq_len(nrow(neighbours))) {
            neighbours[neighbour, ] <- guess
            dimension <- ceiling(neighbour / 2)
            if (neighbour %% 2 == 1) {
                neighbours[neighbour, dimension] <- pmin(
                    guess[dimension] + span * step[dimension],
                    upper_bounds[dimension]
                )
            } else {
                neighbours[neighbour, dimension] <- pmax(
                    guess[dimension] - span * step[dimension],
                    lower_bounds[dimension]
                )
            }

            neighbour_residuals[neighbour] <- .residual(x = x, y = y,
                f = f, pars = neighbours[neighbour, ], n = n, scale = scale,
                family = family,
                trunc = trunc)
        }

        if (min(neighbour_residuals) < guess_residual) {
            guess <- neighbours[which.min(neighbour_residuals)[1], ]
            guess_residual <- min(neighbour_residuals)
        } else {
            span <- span / 2
        }
    }
    return(guess)
}

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
    diffs <- f(x=x, par=pars) - y

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


#' Drop parameters from a function and replace them with constants
#'   inside the function body.
#'
#' @param fn `function` A non-primitive function to remove parameters from
#'   (via `base::formals(fn)`).
#' @param args `list` A list where names are the function arguments (parameters)
#'   to remove and the values are the appopriate value to replace the parameter
#'   with in the function body.
#'
#' @return `function` A new non-primitize function with the parameters named in
#'   `args` deleted and their values fixed with the values from `args` in the
#'   function body.
#'
#' @export
drop_fn_params <- function(fn, args) {
    stopifnot(is.function(fn) && !is.primitive(fn))
    if (length(args) == 0) return(fn)
    stopifnot(all(names(args) %in% formalArgs(fn)))
    stopifnot(is.list(args))
    # Delete the arguments we are deparamterizing
    formals(fn)[formalArgs(fn) %in% names(args)] <- NULL
    # Replace the symbols with the new fixed value inside the fuction body
    deparse_body <- deparse(body(fn))
    for (i in seq_along(args)) {
        deparse_body <- gsub(names(args)[i], args[[i]], deparse_body)
    }
    # Parse the new function body back to a call
    body(fn) <- str2lang(paste0(deparse_body, collapse="\n"))
    # TODO:: Do I need to update the closure environment as well?
    return(fn)
}


#' Collects all function arguments other than the first into a single list
#'   parameter.
#'
#' @description
#' Useful for converting a regular function into a function amenable to optimization
#' via `stats::optim`, which requires all free parameters be passed as a single
#' vector `par`.
#'
#' @details
#' Takes a function of the form f(x, ...), where ... is any number of additional
#'   function parameters (bot not literal `...`!) and parses it to a function of
#'   the form f(par, x) where `par` is a vector of values for ... in
#'   the same order as the arguments appear in `fn`.
#'
#' @param fn `function` A non-primitive function to refactor such that the first
#'   argument becomes the second argument and all other parameters must be
#'   passed as a vector to the first argument of the new function via the `par`
#'   parameter.
#'
#' @return `function` A new non-primitive function where the first argument is
#'   `par`, which takes a vector of parameters being optimized, and the
#'   second argument is the old first argument to `fn` (usually `x` since this
#'   is the independent variable to optimize the function over).
#'
#' @export
collect_fn_params <- function(fn) {
    stopifnot(is.function(fn) && !is.primitive(fn))
    # Capture the current formal args
    formal_args <- formalArgs(fn)
    if ("..." %in% formalArgs(fn))
        stop("No support for fn with ... in signature!")
    # Replace args other than the first with a list
    args <- paste0("par[[", seq_along(formal_args[-1]), "]]") |>
        as.list() |>
        setNames(formal_args[-1])
    fn <- drop_fn_params(fn, args=args)
    # Add the `par` list to the formal args of the function
    formals(fn) <- c(alist(par=), formals(fn))
    return(fn)
}

#' Takes in a loss function and a prediction function to return the loss
#'   of the prediction function.
#'
#' @param loss `function` A numeric loss function. This should take in a numeric
#'   vector of differences and return a single numeric value: the loss for `fn`
#'   on `x` relative to `y`.
#' @param fn `function` A prediction function. Must take in a parameter `x`,
#'   which is a numeric vector to call the function on. Additional args may
#'   by passed via `...`.
#' @param x `numeric()` Independent variable to call `fn` on.
#' @param `y` `numeric()` True values for the dependent variable.
#' @param ... `pairlist` Fallthrough arguments to `fn`.
#'
#' @export
make_loss_fn <- function(loss, fn, x, y, ..., loss_args=list()) {
    stopifnot(is.function(loss) && is.function(fn))
    stopifnot("x" %in% formalArgs(fn))

    loss_fn <- function() {
        diff <- fn(x) - y
        do.call(loss, list(
    }
}


#' Takes a non-primitive R function and refactors it to be compatible with
#'   optimization via `stats::optim`.
#'
#' @description
#'
#'
#' @param fn `function` A non-primitive function
#' @param ... Arguments to `fn` to fix for before building the
#'   function to be optimized. Useful for reducing the number of free parameters
#'   in an optimization if there are insufficient degrees of freedom.
#'
#' @seealso [`drop_fn_params`], [`collect_fn_params`]
#'
#' @export
make_optim_function <- function(fn, ...) {
    # NOTE: error handling done inside helper methods!
    fn1 <- drop_fn_params(fn, args=list(...))
    fn2 <- collect_fn_params(fn1)
    return(fn2)
}


#' Check whether a function signature is amenable to optimization via `stats::optim`.
#'
#' @description
#' Functions compatible with `optim` have the parameter named `par` as their
#' first formal argument where each value is a respective free parameter to
#' be optimized.
#'
#' @param fn `function` A non-primitive function.
#'
#' @return `logical(1)` `TRUE` if the first value of `formalArg(fn)` is "par",
#'   otherwise `FALSE`.
#'
#' @export
is_optim_compatible <- function(fn) formalArgs(fn)[1] == "par"


if (sys.nframe() == 0) {
    source("R/utilities.R")
    source("R/utils-optimization.R")
    # A full function we want to optimize
    hillEqn <- function(x, Emin, Emax, EC50, lambda) {
        (Emin + Emax * (x / EC50)^lambda) / (1 + (x / EC50)^lambda)
    }
    # Set parameters for function testing
    doses <- rev(1000 / (2^(1:20)))
    lambda <- 1
    Emin <- 1
    Emax <- 0.1
    EC50 <- median(doses)

    # We don't want to optimize all the parameters, so lets fix some
    hillEqn2 <- drop_fn_params(hillEqn, args=list(lambda=lambda, Emin=Emin))
    # Now we want to make the function amendable to being called by optim
    hillEqn2Optim <- collect_fn_params(hillEqn2)

    # Helper to combine
    fx <- if (is_optim_compatible(hillEqn)) hillEqn else
        make_optim_function(hillEqn, lambda=lambda, Emin=Emin)

    response <- hillEqn(doses, Emin=Emin, lambda=lambda, Emax=Emax, EC50=EC50)

    # Check equivalence of loss functions
    c1 <- .residual(par=c(0.5, 10), x=doses, y=response, f=fx, family="Cauchy",
        n=1, scale=0.07, trunc=FALSE)
    c2 <- .cauchy_loss(par=c(0.5, 10), x=doses, y=response, f=fx,
        n=1, scale=0.07, trunc=FALSE)
    stopifnot(c1 == c2)
    n1 <-.residual(par=c(0.5, 10), x=doses, y=response, f=fx, family="normal",
        n=1, scale=0.07, trunc=FALSE)
    n2 <- .normal_loss(par=c(0.5, 10), x=doses, y=response, f=fx,
        n=1, scale=0.07, trunc=FALSE)
    stopifnot(n1 == n2)


    opt_pars <- .fitCurve2(
        par=c(Emax=0.5, EC50=10),
        x=doses,
        y=response,
        fn=hillEqn,
        loss=.cauchy_loss,
        loss_args=list(trunc=FALSE, n=1, scale=0.07),
        Emin=Emin,
        lambda=lambda,
        upper=c(2, max(doses)),
        lower=c(0, min(doses)),
        density=c(2, 10),
        precision=1e-4,
        step=0.5 / c(2, 10)
    )

    opt_pars2 <- .fitCurve(
        gritty_guess=c(Emax=0.5, EC50=100),
        x=doses,
        y=response,
        f=fx,
        family="Cauchy",
        trunc=FALSE,
        median_n=1,
        scale=0.07,
        upper_bound=c(2, max(doses)),
        lower_bound=c(0, min(doses)),
        density=c(2, 10),
        precision=1e-4,
        step=0.5 / c(2, 10)
    )

    all.equal(opt_pars, opt_pars2)
}