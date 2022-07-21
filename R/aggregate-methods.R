#' @include LongTable-class.R
#' @include LongTable-accessors.R
#' @include TreatmentResponseExperiment-class.R
NULL

#' @importFrom BiocParallel bpparam bpworkers<- bpworkers bpprogressbar<-
#' bpprogressbar bplapply
#' @importFrom data.table rbindlist setDT merge.data.table
#' @importMethodsFrom S4Vectors aggregate
NULL

#' @noRd
.docs_CoreGx_aggregate <- function(...) CoreGx:::.parseToRoxygen(
    "
    @param by `character` One or more valid column names in `x` to compute
    groups using.
    @param ... `call` One or more aggregations to compute for each group by in x.
    If you name aggregation calls, that will be the column name of the value
    in the resulting `data.table` otherwise a default name will be parsed from
    the function name and its first argument, which is assumed to be the name
    of the column being aggregated over.
    @param nthread `numeric(1)` Number of threads to use for split-apply-combine
    parallelization. Uses `BiocParllel::bplapply` if nthread > 1 or you pass in
    `BPPARAM`. Does not modify data.table threads, so be sure to use
    setDTthreads for reasonable nested parallelism. See details for performance
    considerations.
    @param progress `logical(1)` Display a progress bar for parallelized
    computations? Only works if `bpprogressbar<-` is defined for the current
    BiocParallel back-end.
    @param BPPARAM `BiocParallelParam` object. Use to customized the
    the parallization back-end of bplapply. Note, nthread over-rides any
    settings from BPPARAM as long as `bpworkers<-` is defined for that class.
    @param enlist `logical(1)` Default is `TRUE`. Set to `FALSE` to evaluate
    the first call in `...` within `data.table` groups. See details for more
    information.
    @param moreArgs `list()` A named list where each item is an argument one of
    the calls in `...` which is not a column in the table being aggregated. Use
    to further parameterize you calls. Please note that these are not added
    to your aggregate calls unless you specify the names in the call.

    @details
    ## Use of Non-Standard Evaluation
    Arguments in `...` are substituted and wrapped in a list, which is passed
    through to the j argument of `[.data.table` internally. The function currently
    tries to build informative column names for unnamed arguments in `...` by
    appending the name of each function call with the name of its first argument,
    which is assumed to be the column name being aggregated over. If an argument
    to `...` is named, that will be the column name of its value in the resulting
    `data.table`.

    ## Enlisting
    The primary use case for `enlist=FALSE` is to allow computation of dependent
    aggregations, where the output from a previous aggregation is required in a
    subsequent one. For this case, wrap your call in `{curly}` and assign intermediate
    results to variables, returning the final results as a list where each list
    item will become a column in the final table with the corresponding name.
    Name inference is disabled for this case, since it is assumed you will name
    the returned list items appropriately.
    A major advantage over multiple calls to `aggregate` is that
    the overhead of parallelization is paid only once even for complex multi-step
    computations like fitting a model, capturing its paramters, and making
    predictions using it. It also allows capturing arbitrarily complex calls
    which can be recomputed later using the
    `update,TreatmentResponseExperiment-method`
    A potential disadvantage is increased RAM usage per
    thread due to storing intermediate values in variables, as well as any
    memory allocation overhead associate therewith.
    ",
    ...
)

#' Functional API for aggregation over a `LongTable` or inheriting class
#'
#' @description
#' Compute a group-by operation over a `LongTable` object or it's inhering
#' classes.
#'
#' @param x `LongTable` or inheriting class to compute aggregation on.
#' @param assay `character(1)` The assay to aggregate over.
#' @param subset `call` An R call to evaluate before perfoming an aggregate.
#' This allows you to aggregate over a subset of columns in an assay but have
#' it be assigned to the parent object. Default is TRUE, which includes all
#' rows. Passed through as the `i` argument in `[.data.table`.
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return `data.table` of aggregation results.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @export
setMethod("aggregate", signature(x="LongTable"),
        function(x, assay, by, ...,  subset=TRUE, nthread=1, progress=TRUE,
        BPPARAM=NULL, enlist=TRUE, moreArgs=list()) {
    i <- substitute(subset)
    assay_ <- x[[assay]][eval(i), ]
    aggregate2(
        assay_,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist,
            moreArgs=moreArgs)
})


#' Functional S4 API for aggregation over a `data.table` object.
#'
#' @description
#' Compute a group-by operation over a `data.table` in a functional, pipe
#' compatible format.
#'
#' @details
#' This S4 method override the default `aggregate` method for a `data.frame`
#' and as such you need to call `aggregate.data.frame` directly to get the
#' original S3 method for a `data.table`.
#'
#' @param x `data.table` to compute aggregation over.
#' @param subset `call` An R call to evaluate before perfoming an aggregate.
#' This allows you to aggregate over a subset of columns in an assay but have
#' it be assigned to the parent object. Default is TRUE, which includes all
#' rows. Passed through as the `i` argument in `[.data.table`.
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return `data.table` of aggregated results with an `aggregations` attribute
#' capturing metadata about the last aggregation performed on the table.
#'
#' @export
setMethod("aggregate", signature="data.table",
        function(x, by, ..., subset=TRUE, nthread=1, progress=TRUE,
        BPPARAM=NULL, enlist=TRUE, moreArgs=list()) {
    i <- substitute(subset)
    assay_ <- x[eval(i), ]
    aggregate2(
        x,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist,
            moreArgs=moreArgs)
})

#' Functional API for data.table aggregation which allows capture of associated
#' aggregate calls so they can be recomputed later.
#'
#' @param x `data.table`
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return `data.table` of aggregation results.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @export
aggregate2 <- function(x, by, ..., nthread=1, progress=TRUE, BPPARAM=NULL,
        enlist=TRUE, moreArgs=list()) {
    ## TODO:: refator to checkmate
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))
    stopifnot(is.logical(progress) && length(progress) == 1)
    stopifnot(is.logical(enlist) && length(enlist) == 1)
    stopifnot(is.list(moreArgs))
    stopifnot(length(moreArgs) == 0 || all(names(moreArgs) != ""))

    # -- assign moreArgs to the function scope, it is able to find the values
    for (nm in names(moreArgs)) assign(nm, moreArgs[[nm]])

    # -- capture dots as a call and parse dot names, adding default names if
    # --   they are missing
    agg_call <- if (enlist) substitute(list(...)) else substitute(...)
    if (!enlist && ...length() > 1) warning(.warnMsg("Only one call can be ",
        "passed via ... when enlist=FALSE, ignoring all but first arugment!"))
    dot_names <- if (enlist) names(agg_call)[-1L] else ...names()
    if (is.null(dot_names) && enlist) dot_names <- rep("", length(agg_call) - 1)
    for (i in which(dot_names == "")) {
        dot_call <- agg_call[[i + 1]]
        # assumes the first argument in a function call is always the column name!
        dot_names[i] <- paste0(dot_call[1:max(2, length(dot_call))], collapse="_")
    }
    call_idx <- if (!enlist) 2L else seq(2L, length(agg_call))
    if (length(dot_names)) names(agg_call)[call_idx] <- dot_names

    # -- compute the aggregates, parallelizing if nthread > 1
    if (nthread == 1 && is.null(BPPARAM)) {
        res <- x[, eval(agg_call), by=c(by)]
    } else {
        x <- copy(x) # prevent modifying the source by reference
        # compute groups such that there is one table per thread
        x[, group_id := .GRP, by=by]
        ngrp <- x[, max(group_id)]
        grp_size <- ceiling(ngrp / nthread)
        x[, split_id := ceiling(group_id / grp_size)]
        x_split <- split(x, by="split_id")
        stopifnot(length(x_split) == nthread)
        if (is.null(BPPARAM)) {
            BPPARAM <- BiocParallel::bpparam()
        }
        # optionally add progresbar
        if (hasMethod("bpprogressbar<-", signature=c(class(BPPARAM), "logical"))) {
            BiocParallel::bpprogressbar(BPPARAM) <- progress
        } else if (isTRUE(progress)) {
            warning(.warnMsg(
                "Unable to set progressbar for BiocParallel backend: ",
                class(BPPARAM)[1]), .call=FALSE)
        }
        # optionally set nthread
        if (hasMethod("bpworkers<-", signature=c(class(BPPARAM), "integer"))) {
            BiocParallel::bpworkers(BPPARAM) <- nthread
        } else if (nthread > 1) {
            warning(.warnMsg("Unable to set nthread for BiocParallel backend: ",
                class(BPPARAM)[1]), .call=FALSE)
        }
        res <- BiocParallel::bplapply(
            x_split,
            function(x, agg_call, by) x[, eval(substitute(agg_call)), by=c(by)],
            agg_call=agg_call, by=by,
            BPPARAM=BPPARAM
        )
        res <- rbindlist(res)
    }
    attributes(res)$aggregations <- list(agg_call=agg_call, by=by, enlist=enlist)
    return(res)
}