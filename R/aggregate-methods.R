library(CoreGx)
library(PharmacoGx)
library(data.table)
library(BiocParallel)

#' Functional API for data.table aggregation and capture of associated aggregate
#' calls
#'
#' @param x `data.table`
#' @param by `character` One or more valid column names in `x`.
#' @param ... `call` One or more aggregations to compute for each by in x. If you
#'   name items, that will be the column name of the associated column otherwise
#'   a default name will be parsed from the function and column name.
#' @param nthread `numeric(1)` Number of threads to use for split-apply-combine
#'   parallelization. Uses `BiocParllel::bplapply` if nthread > 1. Does not
#'   modify data.table threads, so be sure to use setDTthreads for reasonable
#'   nested parallelism.
#' @param BPPARAM `BiocParallelParam` object. Use to customized the
#'   the parallization back-end of bplapply. Note, nthread over-rides any
#'   settings from BPPARAM. For now, a progress bar is always used.
#'
#' @details
#' Arguments in ... are substituted and wrapped in a list, which is passed
#' through to the j argument of `[.data.table` interanally. The functin currently
#' tries to build informative column names for unnamed arguments in ... by appending
#' the name of the call with the name of its first argument, which is assumed to
#' be the column name being aggregated over. If an argument to ... is named,
#' that will be the column name for the column for that aggregation.
#'
#' @return `data.table` of aggregation results.
#'
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bplapply
#' @importFrom data.table split rbindlist
#' @export
aggregate2 <- function(x, by, ..., nthread=1, BPPARAM=BiocParallel::bpparam()) {
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))

    # -- capture dots as a call and parse dot names, adding them if they are missing
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
            function(x, agg_call, by) x[, eval(agg_call), by=c(by)],
            agg_call=agg_call, by=by,
            BPPARAM=BPPARAM
        )
        res <- rbindlist(res)
    }
    return(res)
}


if (Sys.frame() == 0) {
    sens <- fread(file.path(".local_data", "sensitivity_assay.csv"))
    # debug(aggregate2)
    sens |>
        aggregate2(
            mv=mean(viability), mean(drug1dose),
            by=c("drug1id", "drug2id", "cellid"),
            nthread=10
        )

    sens[is.na(drug2dose)][drug1id %in% unique(drug1id)[1:5]] |>
        aggregate2(
            auc=tryCatch(computeAUC(drug1dose, viability), error=\(e) NA),
            by=c("drug1id", "cellid")
        ) ->
        auc_dt
}