#' @title lapply
#' lapply method for `MultiAssayExperiment`
#'
#' @param X A `MultiAssayExperiment` object.
#' @param FUN A function to be applied to each `SummarizedExperiment` in a
#' in `X`.
#' @param ... Fall through parameters to `FUN`
#'
#' @return A `MultiAssayExperiment` object, modifed such that
#' `experiments(X) <- endoapply(experiments(X), FUN, ...)`.s
#'
#' @importMethodsFrom BiocGenerics lapply
#' @importFrom S4Vectors endoapply
#' @importFrom MultiAssayExperiment experiments experiments<-
#' @exportMethod lapply
setMethod('lapply', signature(X='MultiAssayExperiment'),
        function(X, FUN, ...) {
    experiments(X) <- endoapply(experiments(X), FUN, ...)
    return(X)
})