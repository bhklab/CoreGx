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
#' assinged to `target`, using `strategy` if `target` is an existing assay in
#' `x`.
#'
#' @seealso `data.table::[.data.table`, `BiocParallel::bplapply`
#'
#' @export
setMethod("endoaggregate", signature(x="LongTable"),
        function(x, ..., assay, target=assay, by, subset=TRUE, nthread=1,
        progress=TRUE, BPPARAM=NULL, enlist=TRUE) {
    i <- substitute(subset)
    assay_ <- x[[assay]][eval(i), ]
    res <- aggregate2(
        assay_,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist
    )
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
            c(PharmacoGx::logLogisticRegression(
                mean_treatment1dose,
                mean_viability),
            list(
                mean_treatment1dose=mean_treatment1dose)
            ),
            by=c("treatment1id", "treatment2id", "sampleid"),
            nthread=20,
            enlist=FALSE
        ) ->
        ntre2
    ntre2 |>
        endoaggregate(
            assay="mono_profiles",
            auc_recomputed = PharmacoGx::computeAUC(
                mean_treatment1dose,
                Hill_fit=c(unique(HS), unique(E_inf), unique(IC50))
            ),
            ic50_recomputed = PharmacoGx::computeIC50(
                mean_treatment1dose,
                Hill_fit=c(unique(HS), unique(E_inf), unique(IC50))
            ),
            by=c("treatment1id", "treatment2id", "sampleid")
        ) ->
        ntre3

}