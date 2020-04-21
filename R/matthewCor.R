## Matthews correlatipon coefficient
#' Compute a Mathews Correlation Coefficient 
#' 
#' The function computes a Matthews correlation coefficient for two factors 
#' provided to the function. It assumes each factor is a factor of class labels, 
#' and the enteries are paired in order of the vectors. 
#' 
#' Please note: we recommend you call set.seed() before using this function to 
#' ensure the reproducibility of your results. Write down the seed number or 
#' save it in a script if you intend to use the results in a publication.
#' 
#' @examples
#' x <- factor(c(1,2,1,2,3,1))
#' y <- factor(c(2,1,1,1,2,2))
#' mcc(x,y)
#' 
#' @param x,y \code{factor} of the same length with the same number of levels
#' @param nperm \code{numeric} number of permutations for significance 
#' estimation. If 0, no permutation testing is done
#' @param nthread \code{numeric} can parallelize permutation texting using 
#'   BiocParallels bplapply
#' @param ... \code{list} Additional arguments
#'   
#' @return A list with the MCC as the $estimate, and p value as $p.value
#' @export
#' 
## TODO:: Give this function a more descriptive name
mcc <- function(x, y, nperm = 1000, nthread = 1, ...) {
    # PARAMETER CHANGE WARNING
    if (!missing(...)) {
        if ("setseed" %in% names(...)) {
            warning("The setseed parameter has been removed in this release to conform
              to Bioconductor coding standards. Please call set.seed in your
              script before running this function.")
        }
    }
    
    if ((length(x) != length(y)) || (!is.factor(x) || length(levels(x)) < 2) || (!is.factor(y) || length(levels(y)) < 2)) {
        stop("x and y must be factors of the same length with at least two levels")
    }
    if (length(levels(x)) != length(levels(y))) {
        
        warning("The number of levels x and y was different. Taking the union of all class labels.")
        levels(x) <- union(levels(x), levels(y))
        levels(y) <- union(levels(x), levels(y))
        
    }
    res <- list(estimate = NA, p.value = NA)
    ## compute MCC
    res[["estimate"]] <- .mcc(ct = table(x, y))
    ## compute significance of MCC using a permutation test
    if (nperm > 0) {
        splitix <- parallel::splitIndices(nx = nperm, ncl = nthread)
        splitix <- splitix[vapply(splitix, length, FUN.VALUE = numeric(1)) > 0]
        mcres <- BiocParallel::bplapply(splitix, function(x, xx, yy) {
            res <- vapply(x, function(x, xx, yy) {
                xx <- sample(xx)
                yy <- sample(yy)
                return(.mcc(ct = table(xx, yy)))
            }, xx = xx, yy = yy, FUN.VALUE = numeric(1))
            return(res)
        }, xx = x, yy = y)
        mcres <- unlist(mcres)
        res[["p.value"]] <- sum(mcres > res["estimate"])/sum(!is.na(mcres))
        if (res["p.value"] == 0) {
            res["p.value"] <- 1/(nperm + 1)
        }
    }
    return(res)
}

## Helper functions


## Just a lot of math, multiclass MCC
## https://en.wikipedia.org/wiki/Matthews_correlation_coefficient#Multiclass_case
.mcc <- 
  function(ct, nbcat=nrow(ct)) {
    if(nrow(ct) != ncol(ct)) { stop("the confusion table should be square!") }

    ## The following code checks if there would be a division by 0 error in the computation on the Matthew's 
    ## correlation coefficient. In practice, this occurs when there are multiple categories but all predictions end up 
    ## in only one category - in this case, the denominator is 0. This is dealt with by adding a psuedocount to each.
    ## This is chosen because the mcc of a matrix of 1s is 0, and such should not affect the value in expectation.  
    ## Note this is not necessary when alll entries lie on the diagonal. 
    if( !(sum(ct)==sum(diag(ct))) && ## Check if all entries are on the diagonal. If they are, no adjustment necessary. 
        ((sum(rowSums(ct) == 0) == (nbcat-1)) || ## Otherwise, check if there is entries only in one column or only in one row. 
        (sum(colSums(ct) == 0) == (nbcat-1)))) {
        ct <- ct + matrix(1, ncol=nbcat, nrow=nbcat) 
      } ### add element to categories if nbcat-1 predictive categories do not contain elements. Not in case where all are correct!
    
    if (sum(ct, na.rm = TRUE) <= 0) {
        return(NA)
    }
    
    myx <- matrix(TRUE, nrow = nrow(ct), ncol = ncol(ct))
    diag(myx) <- FALSE
    if (sum(ct[myx]) == 0) {
        return(1)
    }
    myperf <- 0
    for (k in seq_len(nbcat)) {
        for (m in seq_len(nbcat)) {
            for (l in seq_len(nbcat)) {
                myperf <- myperf + ((ct[k, k] * ct[m, l]) - (ct[l, k] * ct[k, m]))
            }
        }
    }
    aa <- 0
    for (k in seq_len(nbcat)) {
        cc <- 0
        for (l in seq_len(nbcat)) {
            cc <- cc + ct[l, k]
        }
        dd <- 0
        for (f in seq_len(nbcat)) {
            for (g in seq_len(nbcat)) {
                if (f != k) {
                  dd <- dd + ct[g, f]
                }
            }
        }
        aa <- aa + (cc * dd)
    }
    bb <- 0
    for (k in seq_len(nbcat)) {
        cc <- 0
        for (l in seq_len(nbcat)) {
            cc <- cc + ct[k, l]
        }
        dd <- 0
        for (f in seq_len(nbcat)) {
            for (g in seq_len(nbcat)) {
                if (f != k) {
                  dd <- dd + ct[f, g]
                }
            }
        }
        bb <- bb + (cc * dd)
    }
    
    myperf <- myperf/(sqrt(aa) * sqrt(bb))
    return(myperf)
}


