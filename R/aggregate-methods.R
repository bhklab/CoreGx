#' Functional API for data.table aggregation which allows capture of associated
#' aggregate calls so they can be recomputed later.
#'
#' @param x `data.table`
#' @param by `character` One or more valid column names in `x` to compute
#'   groups using.
#' @param ... `call` One or more aggregations to compute for each group by in x.
#'   If you name aggregation calls, that will be the column name of the value
#'   in the resulting `data.table` otherwise a default name will be parsed from
#'   the function name and its first arugment, which is assumed to be the name
#'   of the column being aggregated over.
#' @param nthread `numeric(1)` Number of threads to use for split-apply-combine
#'   parallelization. Uses `BiocParllel::bplapply` if nthread > 1. Does not
#'   modify data.table threads, so be sure to use setDTthreads for reasonable
#'   nested parallelism.
#' @param BPPARAM `BiocParallelParam` object. Use to customized the
#'   the parallization back-end of bplapply. Note, nthread over-rides any
#'   settings from BPPARAM. For now, a progress bar is always used.
#'
#' @details
#' ## Use of Non-Standard Evaluation
#' Arguments in `...` are substituted and wrapped in a list, which is passed
#' through to the j argument of `[.data.table` internally. The functin currently
#' tries to build informative column names for unnamed arguments in `...` by
#' appending the name of each function call with the name of its first argument,
#' which is assumed to be the column name being aggregated over. If an argument
#' to `...` is named, that will be the column name of its value in the resulting
#' `data.table`.
#'
#' ## Parallelization Strategies
#' While your first instinct may be to make use of all available cores, because
#' this method uses `data.table` internally for aggregation the optimal way
#' to compute a set of aggregate functions is dependent on the functions being
#' called. For functions which `data.table` optimizes intenally, such as `mean`,
#' `sd` and other (see `?gforce` for full list of optimized functons)
#'
#' @return `data.table` of aggregation results.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bplapply
#' @importFrom data.table split rbindlist setDT
#' @export
aggregate2 <- function(x, by, ..., nthread=1, BPPARAM=BiocParallel::bpparam()) {
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))

    # -- capture dots as a call and parse dot names, adding default names if
    # --   they are missing
    agg_call <- substitute(list(...))
    dot_names <- names(agg_call)[-1L]
    if (is.null(dot_names)) dot_names <- rep(TRUE, length(agg_call) - 1)
    for (i in which(dot_names == "")) {
        dot_call <- agg_call[[i + 1]]
        # assumes the first argument in a function call is always the column name!
        dot_names[i] <- paste0(dot_call[1:max(2, length(dot_call))], collapse="_")
    }
    names(agg_call)[2L:length(agg_call)] <- dot_names

    # -- compute the aggregates, parallelizing if nthread > 1
    if (nthread == 1) {
        res <- x[, eval(agg_call), by=c(by)]
    } else {
        x_split <- data.table:::split.data.table(x, by=by)
        BiocParallel::bpworkers(BPPARAM) <- nthread
        BiocParallel::bpprogressbar(BPPARAM) <- TRUE
        res <- BiocParallel::bplapply(
            x_split,
            function(x, agg_call, by) x[, eval(substitute(agg_call)), by=c(by)],
            agg_call=agg_call, by=by,
            BPPARAM=BPPARAM
        )
        res <- rbindlist(res)
    }
    return(res)
}


if (Sys.frame() == 0) {
    library(CoreGx)
    library(PharmacoGx)
    library(data.table)
    library(BiocParallel)
    sens <- fread(file.path(".local_data", "sensitivity_assay.csv"))
    # debug(aggregate2)
    sens[is.na(drug2dose)] |>
        aggregate2(
            mv=mean(viability), mean(drug1dose),
            by=c("drug1id", "drug2id", "cellid"),
            nthread=10
        )
    bench::system_time({
        sens |>
            subset(is.na(drug2dose)) |>
            aggregate2(
                auc=computeAUC(drug1dose, viability),
                by=c("drug1id", "cellid"),
                nthread=22
            ) ->
            auc_dt
    })
}