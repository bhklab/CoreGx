#' GWC Score
#' 
#' Calculate the gwc score between two vectors, using either a weighted spearman 
#'  or pearson correlation
#'
#' @examples
#' data(clevelandSmall)
#' x <- molecularProfiles(clevelandSmall,"rna")[,1]
#' y <- molecularProfiles(clevelandSmall,"rna")[,2]
#' x_p <- rep(0.05, times=length(x))
#' y_p <- rep(0.05, times=length(y))
#' names(x_p) <- names(x)
#' names(y_p) <- names(y)
#' gwc(x,x_p,y,y_p, nperm=100)
#'
#'@param x1 \code{numeric} vector of effect sizes (e.g., fold change or t statitsics) for the first experiment
#'@param p1 \code{numeric} vector of p-values for each corresponding effect size for the first experiment
#'@param x2 \code{numeric} effect size (e.g., fold change or t statitsics) for the second experiment
#'@param p2 \code{numeric} vector of p-values for each corresponding effect size for the second experiment
#'@param method.cor \code{character} string identifying if a \code{pearson} or
#'\code{spearman} correlation should be used
#'@param nperm \code{numeric} how many permutations should be done to determine
#'@param truncate.p \code{numeric} Truncation value for extremely low p-values
#'@param ... Other passed down to internal functions
#'
#'@return \code{numeric} a vector of two values, the correlation and associated p-value.
#'@export
##            -
##
## http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient#Calculating_a_weighted_correlation
## http://www.mathworks.com/matlabcentral/fileexchange/20846-weighted-correlation-matrix
## F. Pozzi, T. Di Matteo, T. Aste, "Exponential smoothing weighted correlations", The European Physical Journal B, Vol. 85, No 6, 2012. DOI: 10.1140/epjb/e2012-20697-x
#################################################
##TODO:: Give this function a more descriptive name
gwc <-
function (x1, p1, x2, p2, method.cor=c("pearson", "spearman"), nperm=1e4, truncate.p=1e-16, ...) {
    
    method.cor <- match.arg(method.cor)
    ## intersection between x and y
    ii <- intersectList(names(x1), names(p1), names(x2), names(p2))
    if(length(ii) < 10) {
        stop ("Less than 10 probes/genes in common between x and y")
    }
    x1 <- x1[ii]
    p1 <- p1[ii]
    x2 <- x2[ii]
    p2 <- p2[ii]
    ## truncate extremely low p-values
    p1[!is.na(p1) & p1 < truncate.p] <- truncate.p
    p2[!is.na(p2) & p2 < truncate.p] <- truncate.p
    ## scaled weights
    p1 <- -log10(p1)
    p1 <- p1 / sum(p1, na.rm=TRUE)
    p2 <- -log10(p2)
    p2 <- p2 / sum(p2, na.rm=TRUE)
    w <- p1 + p2
    ## compute genome-wide connectivity score
    res <- .corWeighted(x=x1, y=x2, w=w, method=method.cor, nperm=nperm, ...)
    return(res)
}

#################################################
## Compute a weighted correlation coefficient
## inspired from package boot
##
## inputs:    
##      - 
##
## outputs, a list of items:
##            - 
##
#################################################
##TODO:: Write function documentation?
#
#' @importFrom stats cov.wt complete.cases
.corWeighted <- 
    function (x, y, w, method=c("pearson", "spearman"), 
              alternative=c("two.sided", "greater", "less"), 
              nperm=0, nthread=1, na.rm=FALSE)
    {
        
        ######################
        wcor <- function (d, w, na.rm=TRUE) {
            ## NOTE:: THIS FORMULA CAN SUFFER CATASTROPHIC CANCELATION AND SHOULD BE FIXED!
            #     s <- sum(w, na.rm=na.rm)
            #     m1 <- sum(d[ , 1L] * w, na.rm=na.rm) / s
            #     m2 <- sum(d[ , 2L] * w, na.rm=na.rm) / s
            #     res <- (sum(d[ , 1L] * d[ , 2L] * w, na.rm=na.rm) / s - m1 * m2) / 
            #sqrt((sum(d[ , 1L]^2 * w, na.rm=na.rm) / s - m1^2) * (sum(d[ , 2L]^2 * w, 
            #na.rm=na.rm) / s - m2^2))
            CovM <- cov.wt(d, wt=w)[["cov"]]
            res <- CovM[1,2]/sqrt(CovM[1,1]*CovM[2,2])
            return(res)
        }
        
        ######################
        
        if (missing(w)) { w <- rep(1, length(x)) / length(x) }
        if (length(x) != length(y) || length(x) != length(w)) { stop("x, y, and w must have the same length") }
        method <- match.arg(method)
        if (method == "spearman") {
            x <- rank(x)
            y <- rank(y)
        }
        alternative <- match.arg(alternative)
        
        res <- c("rho"=NA, "p"=NA)
        
        ## remove missing values
        ccix <- complete.cases(x, y, w)
        if(!all(ccix) && !na.rm) { warning("Missing values are present") }
        if(sum(ccix) < 3) {
            return(res)
        }
        x <- x[ccix]
        y <- y[ccix]
        w <- w[ccix]
        
        wc <- wcor(d=cbind(x, y), w=w)
        res["rho"] <- wc
        if (nperm > 1) {
            splitix <- parallel::splitIndices(nx=nperm, ncl=nthread)
            if (!is.list(splitix)) { splitix <- list(splitix) }
            splitix <- splitix[vapply(splitix, length, FUN.VALUE=numeric(1)) > 0]
            mcres <- parallel::mclapply(splitix, function(x, xx, yy, ww) {
                pres <- vapply(x, function(x, xx, yy, ww) {
                    ## permute the data and the weights
                    d2 <- cbind(xx[sample(seq_len(length(xx)))], yy[sample(seq_len(length(yy)))])
                    w2 <- ww[sample(seq_len(length(ww)))]
                    return(wcor(d=d2, w=w2))
                }, xx=xx, yy=yy, ww=ww, FUN.VALUE=double(1))
                return(pres)
            }, xx=x, yy=y, ww=w)
            perms <- do.call(c, mcres)
            
            switch (alternative,
                    "two.sided" = { 
                        if (res["rho"] < 0) { p <- sum(perms <= res, na.rm=TRUE) } else { p <- sum(perms >= res, na.rm=TRUE) }
                        if (p == 0) { p <- 1 / (nperm + 1) } else { p <- p / nperm }
                        p <- p * 2
                    },
                    "greater" = {
                        p <- sum(perms >= res, na.rm=TRUE) 
                        if (p == 0) { p <- 1 / (nperm + 1) } else { p <- p / nperm }
                    },
                    "less" = {
                        p <- sum(perms <= res, na.rm=TRUE) 
                        if (p == 0) { p <- 1 / (nperm + 1) } else { p <- p / nperm }
                    })
            res["p"] <- p
        }
        return(res)
    }

