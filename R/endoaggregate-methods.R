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
## TODO:: Do we need this parameter? Is there any case where a join wouldn't work?
#' @param strategy `character(1)` Assuming `target` is an existing assay
#' in the `LongTable`, how should the assays be merged? Options are `cbind`,
#' "rbind" or "merge". For strategy "merge", the join will be done using the
#' `by` columns as the key. Defaults to "merge".
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return Object with the same class as `x`, with the aggregation results
#' assinged to `target`, using `strategy` if `target` is an existing assay in
#' `x`.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @export
setMethod("endoaggregate", signature(x="LongTable"),
        function(x, ..., assay, target=assay, by, subset=TRUE, nthread=1,
        progress=TRUE, BPPARAM=NULL, enlist=TRUE,
        strategy=c("merge", "rbind", "cbind")) {
    i <- substitute(subset)
    assay_ <- x[[assay]][eval(i), ]
    res <- aggregate2(
        assay_,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist)
    strategy <- match.arg(strategy)
    if (target %in% assayNames(x)) {
        res <- merge.data.table(x[[assay]], res, by=by)
    }
    x[[target]] <- res
    x
})

if (sys.nframe() == 0) {
    library(CoreGx)
    library(PharmacoGx)

    nci <- readRDS(file.path(".local_data", "NCI_ALMANAC_2017.rds"))
    tre <- treatmentResponse(nci)
    # test adding new assay
    tre |>
        endoaggregate(
            assay="sensitivity",
            target="sensitivity_no_reps",
            mean(treatment1dose),
            mean(treatment2dose),
            mean(viability),
            by=c("treatment1id", "treatment1dose", "treatment2id", "treatment2dose", "sampleid")
        ) ->
        ntre
    ntre |>
        endoaggregate(
            assay="sensitivity_no_reps",
            target="mono_profiles",
            subset=treatment2id == "",
            PharmacoGx::logLogisticRegression(
                mean_treatment1dose,
                mean_viability
            ),
            by=c("treatment1id", "treatment2id", "sampleid"),
            nthread=20,
            enlist=FALSE
        ) ->
        ntre2

}