#' @include allGenerics.R
#' @include aggregate-methods.R
NULL

#' Functional API for endomorphic aggregation over a `LongTable` or
#' inheriting class
#'
#' @description
#' Compute a group-by operation over a `LongTable` object or its inhering
#' classes.
#'
#' @param x `LongTable` or inheriting class to compute aggregation on.
#' @param assay `character(1)` The assay to aggregate over.
#' @param target `character(1)` The assay to assign the results to. Defaults
#' to `assay`.
#' @param subset `call` An R call to evaluate before perfoming an aggregate.
#' This allows you to aggregate over a subset of columns in an assay but have
#' it be assigned to the parent object. Default is TRUE, which includes all
#' rows. Passed through as the `i` argument in `[.data.table`.
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return Object with the same class as `x`, with the aggregation results
#' assigned to `target`, using `strategy` if `target` is an existing assay in
#' `x`.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @export
setMethod("endoaggregate", signature(x="LongTable"),
        function(x, ..., assay, target=assay, by, subset=TRUE, nthread=1,
        progress=TRUE, BPPARAM=NULL, enlist=TRUE, moreArgs=list()) {
    i <- substitute(subset)
    assay_ <- x[[assay]][eval(i), ]
    res <- aggregate2(
        assay_,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist,
        moreArgs=moreArgs
    )
    if (target %in% assayNames(x)) {
        res <- merge.data.table(x[[assay]], res, by=by)
    }
    x[[target]] <- res
    x
})
