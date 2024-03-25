#' Drug sensitivity calling using waterfall plots
#'
#' 1. Sensitivity calls were made using one of IC50, ActArea or Amax
#'
#' 2. Sort log IC50s (or ActArea or Amax) of the samples to generate a
#'   “waterfall distribution”
#'
#' 3. Identify cutoff:
#'
#'  3.1 If the waterfall distribution is non-linear (pearson cc to the linear
#'    fit <=0.95), estimate the major inflection point of the log IC50 curve as
#'    the point on the curve with the maximal distance to a line drawn between
#'    the start and end points of the distribution.
#'
#'  3.2 If the waterfall distribution appears linear (pearson cc to the linear
#'    fit > 0.95), then use the median IC50 instead.
#'
#' 4. Samples within a 4-fold IC50 (or within a 1.2-fold ActArea or 20% Amax
#'   difference) difference centered around this inflection point are classified
#'   as being “intermediate”,  samples with lower IC50s (or ActArea/Amax
#'   values) than this range are defined as sensitive, and those with IC50s (or
#'   ActArea/Amax) higher than this range are called “insensitive”.
#'
#' 5. Require at least x sensitive and x insensitive samples after applying
#'   these criteria (x=5 in our case).
#'
## FIXME:: Write a real example
#' @examples
#' # Dummy example
#' 1 + 1
#'
## FIXME:: Clarify the parameters of this function
#' @param x What type of object does this take in?
#' @param type
#'   ic50: IC50 values in micro molar (positive values)
#'   actarea: Activity Area, that is area under the drug activity curve (positive values)
#'   amax: Activity at max concentration (positive values)
#'
#' @param intermediate.fold vector of fold changes used to define the intermediate sensitivities for ic50, actarea and amax respectively
#' @param cor.min.linear \code{numeric} The minimum linear correlation to
#'   require?
#' @param name \code{character} The name of the output to use in plot
#' @param plot \code{boolean} Whether to plot the results
#'
#' @return \code{factor} Containing the drug sensitivity status of each
#'   sample.
#'
#' @importFrom stats complete.cases  cor.test lm median
#' @importFrom graphics par points abline lines legend
#' @importFrom grDevices rainbow
#'
#' @export
#' @keywords internal
callingWaterfall <- function(
    x, 
    type = c("IC50", "AUC", "AMAX"), 
    intermediate.fold = c(4, 1.2, 1.2), 
    cor.min.linear = 0.95, 
    name = "Drug",
    plot = FALSE
) {
    funContext <- .funContext("::callingWaterfall")

    # Set the 'type' variable to one of the allowed values
    # type <- match.arg(type)
    type <- "IC50"
    .message(funContext, "Calling waterfall for ", type, " with ", name)

    # If there are any negative values in 'intermediate.fold', replace them with 0
    if (any(!is.na(intermediate.fold) & intermediate.fold < 0)) {
        intermediate.fold <- intermediate.fold[!is.na(intermediate.fold) & intermediate.fold < 0] <- 0
    }

    # If the 'names' of 'x' are not defined, assign them as "X.1", "X.2", etc.
    if (is.null(names(x))) {
        names(x) <- paste("X", seq_along(x), sep = ".")
        .message(funContext, "No names in input data, using ", length(x), " default names")
    }

    xx <- x[complete.cases(x)]
    switch(type, IC50 = {
        xx <- -log10(xx)
        ylabel <- "-log10(IC50)"
        ## 4 fold difference around IC50 cutoff
        if (length(intermediate.fold) == 3) {
            intermediate.fold <- intermediate.fold[1]
        }
        if (intermediate.fold != 0) {
            interfold <- log10(intermediate.fold)
        } else {
            interfold <- 0
        }
    }, AUC = {
        ylabel <- "AUC"
        ## 1.2 fold difference around Activity Area cutoff
        if (length(intermediate.fold) == 3) {
            intermediate.fold <- intermediate.fold[2]
        }
        interfold <- intermediate.fold
    }, AMAX = {
        ylabel <- "Amax"
        ## 1.2 fold difference around Amax
        if (length(intermediate.fold) == 3) {
            intermediate.fold <- intermediate.fold[3]
        }
        interfold <- intermediate.fold
    })

    # Check if the length of 'xx' is less than 3
    if (length(xx) < 3) {
        tt <- array(NA, dim = length(x), dimnames = list(names(x)))
        # Create an array 'tt' with NA values, having the same length as 'x'
        # The dimensions of 'tt' are set using the length of 'x' and the names of 'x'
        
        if (interfold == 0) {
            # Check if 'interfold' is equal to 0
            tt <- factor(tt, levels = c("resistant", "sensitive"))
            # Convert 'tt' into a factor with levels "resistant" and "sensitive"
        } else {
            tt <- factor(tt, levels = c("resistant", "intermediate", "sensitive"))
            # Convert 'tt' into a factor with levels "resistant", "intermediate", and "sensitive"
        }
        
        return(tt)
        # Return the 'tt' array
    }


    oo <- order(xx, decreasing = TRUE)
    ## test linearity with Pearson correlation
    cc <- stats::cor.test(-xx[oo], seq_along(oo), method = "pearson")
    ## line between the two extreme sensitivity values
    dd <- cbind(y = xx[oo][c(1, length(oo))], x = c(1, length(oo)))
    rr <- lm(y ~ x, data = data.frame(dd))
    ## compute distance from sensitivity values and the line between the two extreme sensitivity values
    ddi <- apply(cbind(seq_along(oo), xx[oo]), 1, function(x, slope, intercept) {
        return(.distancePointLine(x = x[1], y = x[2], a = slope, b = intercept))
    }, slope = rr$coefficients[2], intercept = rr$coefficients[1])
    if (cc$estimate > cor.min.linear) {
        ## approximately linear waterfall
        msg <- sprintf("Linear waterfall with R=%.3g", cc$estimate)
        .message(funContext, msg)
        msg2 <- sprintf("Using median as cutoff")
        .message(funContext, msg2)
        
        cutoff_attribute <- "median"
        cutoff <- which.min(abs(xx[oo] - median(xx[oo])))
        cutoffn <- names(cutoff)[1]
    } else {
        ## non linear waterfall identify cutoff as the maximum distance
        msg <- sprintf("Non-linear waterfall with R=%.3g", cc$estimate)
        .message(funContext, msg)
        msg2 <- sprintf("Using maximal distance as cutoff")
        .message(funContext, msg2)
        
        cutoff_attribute <- "maximal distance"
        cutoff <- which.max(abs(ddi))
        cutoffn <- names(ddi)[cutoff]
    }


    ## identify intermediate sensitivities
    switch(type, IC50 = {
        if (interfold == 0) {
            rang <- c(xx[oo][cutoff], xx[oo][cutoff])
        } else {
            rang <- c(xx[oo][cutoff] - interfold, xx[oo][cutoff] + interfold)
        }
    }, AUC = {
        if (interfold == 0) {
            rang <- c(xx[oo][cutoff], xx[oo][cutoff])
        } else {
            rang <- c(xx[oo][cutoff]/interfold, xx[oo][cutoff] * interfold)
        }
    }, AMAX = {
        if (interfold == 0) {
            rang <- c(xx[oo][cutoff], xx[oo][cutoff])
        } else {
            rang <- c(xx[oo][cutoff]/interfold, xx[oo][cutoff] * interfold)
        }
    })


    ## check whether range is either min or max
    if (rang[2] >= max(xx)) {
        rang[2] <- sort(unique(xx), decreasing = TRUE)[2]
    }
    if (rang[2] <= min(xx)) {
        rang[2] <- sort(unique(xx), decreasing = FALSE)[2]
    }
    if (rang[1] <= min(xx)) {
        rang[1] <- sort(unique(xx), decreasing = FALSE)[2]
    }
    if (rang[1] >= max(xx)) {
        rang[1] <- sort(unique(xx), decreasing = TRUE)[2]
    }

    ## compute calls
    calls <- rep(NA, length(xx))
    names(calls) <- names(xx)
    calls[xx < rang[1]] <- "resistant"
    calls[xx >= rang[2]] <- "sensitive"
    calls[xx >= rang[1] & xx < rang[2]] <- "intermediate"

    if (plot) {
        par(mfrow = c(2, 1))
        ccols <- rainbow(4)
        mycol <- rep("grey", length(xx))
        names(mycol) <- names(xx)
        mycol[calls == "sensitive"] <- ccols[2]
        mycol[calls == "intermediate"] <- ccols[3]
        mycol[calls == "resistant"] <- ccols[4]
        mycol[cutoffn] <- ccols[1]
        mypch <- rep(16, length(xx))
        names(mypch) <- names(xx)
        mypch[cutoffn] <- 19
        plot(xx[oo], col = mycol[oo], pch = mypch[oo], ylab = ylabel, main = sprintf("%s\nWaterfall", name))
        points(x = cutoff, y = xx[cutoffn], pch = mypch[cutoffn], col = mycol[cutoffn])
        graphics::abline(a = rr$coefficients[1], b = rr$coefficients[2], lwd = 2, col = "darkgrey")
        lines(x = c(cutoff, cutoff), y = c(par("usr")[3], xx[cutoffn]), col = "red")
        lines(x = c(par("usr")[1], cutoff), y = c(xx[cutoffn], xx[cutoffn]), col = "red")
        legend("topright", legend = c(sprintf("resistant (n=%i)", sum(!is.na(calls) & calls == "resistant")), sprintf("intermediate (n=%i)",
            sum(!is.na(calls) & calls == "intermediate")), sprintf("sensitive (n=%i)", sum(!is.na(calls) & calls == "sensitive")), "cutoff",
            sprintf("R=%.3g", cc$estimate)), col = c(rev(ccols), NA), pch = c(16, 16, 16, 19, NA), bty = "n")

        plot(ddi, pch = mypch[oo], col = mycol[oo], ylab = "Distance", main = sprintf("%s\n%s", name, "Distance from min--max line"))
        points(x = cutoff, y = ddi[cutoffn], pch = mypch[cutoffn], col = mycol[cutoffn])
        legend("topright", legend = c("resistant", "intermediate", "sensitive", "cutoff"), col = rev(ccols), pch = c(16, 16, 16, 19), bty = "n")
    }

    tt <- rep(NA, length(x))
    names(tt) <- names(x)
    tt[names(calls)] <- calls
    if (interfold == 0) {
        tt <- factor(tt, levels = c("resistant", "sensitive"))
    } else {
        tt <- factor(tt, levels = c("resistant", "intermediate", "sensitive"))
    }

    # Add attributes to the output
    attr(tt, "parameters") <- list(
        type = type,
        intermediate.fold = interfold,
        cor.min.linear = cor.min.linear
    )

    attr(tt, "cutoff") <- list(
        method = cutoff_attribute,
        value = xx[oo][cutoff],
        distance = ddi[cutoffn]
    )

    return(tt)
}


# Helper Functions --------------------------------------------------------

#' Calculate shortest distance between point and line
#'
#'
#' @description This function calculates the shortest distance between a point
#'   and a line in 2D space.
#'
#' @param x x-coordinate of point
#' @param y y-coordinate of point
#' @param a `numeric(1)` The coefficient in line equation a * x + b * y + c = 0.
#'   Defaults to 1.
#' @param b `numeric(1)` The coefficient in line equation a * x + b * y + c = 0.
#'   Defaults to 1.
#' @param c `numeric(1)` The intercept in line equation a * x + b * y + c = 0.
#'   Defaults to 0.
#'
#' @return `numeric` The shortest distance between a point and a line.
#'
#' @examples 
#' .distancePointLine(0, 0, 1, -1, 1)
#' 
#' @export
#' @keywords internal
.distancePointLine <- function(x, y, a=1, b=1, c=0) {

    if (!(all(is.finite(c(x, y, a, b, c))))) {
        stop("All inputs to .distancePointLine must be real numbers.")
    }

    return(abs(a * x + b * y + c) / sqrt(a^2 + b^2))
}

#' @export
#' @keywords internal
.magnitude <- function(p1, p2) {
    return(sqrt(sum((p2 - p1)^2)))
}

#' Calculate shortest distance between point and line segment
#'
#' @description This function calculates the shortest distance between a point
#'   and a line segment in 2D space.
#'
#' @param x x-coordinate of point
#' @param y y-coordinate of point
#' @param x1 x-coordinate of one endpoint of the line segment
#' @param y1 y-coordinate of line segment endpoint with x-coordinate x1
#' @param x2 x-coordinate of other endpoint of line segment
#' @param y2 y-coordinate of line segment endpoint with x-coordinate x2
#'
#' @return \code{numeric} The shortest distance between a point and a line
#'   segment
#'
#' @examples .distancePointSegment(0, 0, -1, 1, 1, -1)
#'
#' @export
#' @keywords internal
.distancePointSegment <- function(x, y, x1, y1, x2, y2) {
    if (!(all(is.finite(c(x, y, x1, x2, y1, y2))))) {
        stop("All inputs to linePtDist must be real numbers.")
    }

    bestEndpointDistance <- min(c(.magnitude(c(x, y), c(x1, y1)), .magnitude(c(x, y), c(x2, y2))))

    if (.magnitude(c(x, y), c((x1 + x2)/2, (y1 + y2)/2)) < bestEndpointDistance) {
        # length to point has only one local minimum which is the global minimum; iff this condition is true then the shortest distance is to a
        # point on the line segment vertical line segment
        if (x1 == x2) {
            a <- 1
            b <- 0
            c <- -x1
        } else {
            a <- (y2 - y1)/(x2 - x1)
            b <- -1
            c <- y1 - a * x1
        }
        return(.distancePointLine(x, y, a, b, c))
    } else {
        return(bestEndpointDistance)
    }
}
