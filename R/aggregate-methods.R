#' @include LongTable-class.R
#' @include LongTable-accessors.R
#' @include TreatmentResponseExperiment-class.R
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
    parallelization. Uses `BiocParllel::bplapply` if nthread > 1. Does not
    modify data.table threads, so be sure to use setDTthreads for reasonable
    nested parallelism. See details for performance considerations.
    @param BPPARAM `BiocParallelParam` object. Use to customized the
    the parallization back-end of bplapply. Note, nthread over-rides any
    settings from BPPARAM. For now, a progress bar is always used. If this has
    negatively affected your mental health please feel free to open an issue
    on GitHub at bhklab/CoreGx and we will parameterize it.
    @param enlist `logical(1)` Default is `TRUE`. Set to `FALSE` to evaluate
    the first call in `...` within `data.table` groups. See details for more
    information.

    @details
    ## Use of Non-Standard Evaluation
    Arguments in `...` are substituted and wrapped in a list, which is passed
    through to the j argument of `[.data.table` internally. The function currently
    tries to build informative column names for unnamed arguments in `...` by
    appending the name of each function call with the name of its first argument,
    which is assumed to be the column name being aggregated over. If an argument
    to `...` is named, that will be the column name of its value in the resulting
    `data.table`.


    ## Parallelization Strategies
    While your first instinct may be to make use of all available cores, because
    this method uses `data.table` internally for aggregation the optimal way
    to compute a set of aggregate functions is dependent on the functions being
    called. For functions which `data.table` optimizes intenally, such as `mean`,
    `sd` and others (see `?gforce` for full list of optimized functons) it is
    almost always better to run this function with `nthread=1` and let
    `data.table` handle parallelization.
    However, for functions not internally optimized by `data.table`, such as
    fitting statistical models, compute time can be reduced by adding
    more cores at the cost of additional memory usage.

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
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return `data.table` of aggregation results.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bplapply
#' @importFrom data.table rbindlist setDT
#' @importMethodsFrom S4Vectors aggregate
#' @export
setMethod("aggregate", signature(x="LongTable"),
        function(x, assay, by, ..., nthread=1, BPPARAM=NULL, enlist=TRUE) {
    aggregate2(
        x[[assay]],
        by=by,
        ...,
        nthread=nthread, BPPARAM=BPPARAM, enlist=enlist)
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
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bplapply
#' @importFrom data.table rbindlist setDT
#' @export
aggregate2 <- function(x, by, ..., nthread=1, BPPARAM=NULL, enlist=TRUE) {
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))

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
    if (nthread == 1) {
        res <- x[, eval(agg_call), by=c(by)]
    } else {
        x_split <- split(x, by=by)
        if (is.null(BPPARAM)) BPPARAM <- BiocParallel::bpparam()
        if (hasMethod("bpworkers<-", signature=c(class(BPPARAM), "integer")))
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
    attributes(res)$aggregations <- list(agg_call=agg_call, by=by, enlist=enlist)
    return(res)
}



## TESTING CODE
if (sys.nframe() == 0) {
    library(CoreGx)
    library(PharmacoGx)
    library(data.table)
    library(BiocParallel)
    library(doParallel)

    # Load example assay
    sens <- fread(file.path(".local_data", "sensitivity_assay.csv"))
    tre <- qs::qread(
        file.path(".local_data", "nci_treatment_response_exp.qs"),
        nthread=10
    )

    ## TreatmentResponseExperiment method
    bench::system_time({
        tre |>
            subset(is.na(drug2dose)) |>
            aggregate(
                assay="sensitivity",
                auc=PharmacoGx::computeAUC(drug1dose, viability),
                by=c("drug1id", "cellid"),
                nthread=22
            ) ->
            profiles
    })

    # debug(aggregate2)

    # data.table aggregation
    sens[is.na(drug2dose)] |>
        aggregate2(
            mv=mean(viability), mean(drug1dose),
            by=c("drug1id", "drug2id", "cellid")
        )

    ## MultiCoreParam
    bp <- bpparam()
    bench::system_time({
        sens |>
            subset(is.na(drug2dose)) |>
            aggregate2(
                auc=PharmacoGx::computeAUC(drug1dose, viability),
                by=c("drug1id", "cellid"),
                BPPARAM=bp, nthread=22
            ) ->
            auc_dt
    })

    ## SnowParam
    bp1 <- SnowParam()
    bench::system_time({
        sens |>
            subset(is.na(drug2dose)) |>
            aggregate2(
                auc=PharmacoGx::computeAUC(drug1dose, viability),
                by=c("drug1id", "cellid"),
                BPPARAM=bp1, nthread=22
            ) ->
            auc_dt
    })

    ## DoParParam - should be faster than MultiCoreParam
    registerDoParallel(22)
    bp2 <- DoparParam()

    bench::system_time({
        sens |>
            subset(is.na(drug2dose)) |>
            aggregate2(
                auc=computeAUC(drug1dose, viability),
                by=c("drug1id", "cellid"),
                BPPARAM=bp2, nthread=22
            ) ->
            auc_dt
    })

    ## Apples to apples comparison (hopefully)
    bmark <- bench::mark(
        mutl={
            sens |>
                subset(is.na(drug2dose)) |>
                aggregate2(
                    auc=computeAUC(drug1dose, viability),
                    by=c("drug1id", "cellid"),
                    BPPARAM=bp, nthread=22
                ) ->
                auc_dt
        },
        dopar={
            sens |>
                subset(is.na(drug2dose)) |>
                aggregate2(
                    auc=computeAUC(drug1dose, viability),
                    by=c("drug1id", "cellid"),
                    BPPARAM=bp2, nthread=22
                ) ->
                auc_dt
        }
    )

library(CoreGx)
data(nci_TRE_small)
# fix use of wrong column for sensitivity
nci_TRE_small$sensitivity <- nci_TRE_small$sensitivity[,
    `:=`(PERCENTGROWTH=viability, viability=PERCENTGROWTHNOTZ)
]
nci_TRE_small |>
    subset(is.na(drug2id)) |>
    aggregate(
        assay="sensitivity",
        {
            fit <- tryCatch({
                PharmacoGx::logLogisticRegression(drug1dose, viability)
            }, error=\(e) list(HS=NA_real_, E_inf=NA_real_, EC50=NA_real_))
            ic50 <- tryCatch({
                PharmacoGx::computeIC50(drug1dose, Hill_fit=fit)
            }, error=\(e) NA_real_)
            auc <- tryCatch({
                PharmacoGx::computeAUC(drug1dose, Hill_fit=fit, area.type="Fitted")
            }, error=\(e) NA_real_)
            list(
                HS=fit[['HS']], E_inf=fit[['E_inf']], EC50=fit[['EC50']],
                auc=auc,
                ic50=ic50
            )
        },
        by=c("drug1id", "cellid"),
        enlist=FALSE,
        nthread=22
    ) -> profiles
nci_TRE_small$mono_profiles <- profiles


}