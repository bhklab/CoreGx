#' @include LongTable-class.R
#' @include LongTable-accessors.R
#' @include TreatmentResponseExperiment-class.R
NULL

#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bplapply
#' @importFrom data.table rbindlist setDT
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
#' @export
setMethod("aggregate", signature(x="LongTable"),
        function(x, assay, by, ..., nthread=1, progress=TRUE, BPPARAM=NULL,
        enlist=TRUE) {
    aggregate2(
        x[[assay]],
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist)
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
#' @eval .docs_CoreGx_aggregate(curly="{")
#'
#' @return `data.table` of aggregated results with an `aggregations` attribute
#' capturing metadata about the last aggregation performed on the table.
#'
#' @export
setMethod("aggregate", signature="data.table",
        function(x, by, ..., nthread=1, progress=TRUE, BPPARAM=NULL, enlist=TRUE) {
    aggregate2(
        x,
        by=by,
        ...,
        nthread=nthread, progress=progress, BPPARAM=BPPARAM, enlist=enlist)
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
        enlist=TRUE) {
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))
    stopifnot(is.logical(progress) && length(progress) == 1)
    stopifnot(is.logical(enlist) && length(enlist) == 1)

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
        x_split <- split(x, by=by)
        if (is.null(BPPARAM)) {
            BPPARAM <- BiocParallel::bpparam()
        }
        # optionally add progresbar
        if (hasMethod("bpprogressbar<-", signature=c(class(BPPARAM), "logical"))) {
            BiocParallel::bpprogressbar(BPPARAM) <- progress
        } else if (isTRUE(progress)) {
            warning(.warnMsg(
                "Unable to set progressbar for BiocParallel backend: ",
                class(BPPARAM)), .call=FALSE)
        }
        # optionally set nthread
        if (hasMethod("bpworkers<-", signature=c(class(BPPARAM), "integer"))) {
            BiocParallel::bpworkers(BPPARAM) <- nthread
        } else if (nthread > 1) {
            warning(.warnMsg("Unable to set nthread for BiocParallel backend: ",
                class(BPPARAM)), .call=FALSE)
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
                nthread=6
            ) ->
            profiles
    })

        bench::system_time({
tre |>
    subset(is.na(drug2dose)) |>
    aggregate(
        assay="sensitivity",
        if (.N > 3) .SD,
        by=c("drug1id", "cellid"),
        nthread=1,
        enlist=FALSE
    ) ->
    test
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
    library(data.table)
    tre <- qs::qread(
        file.path(".local_data", "nci_treatment_response_exp.qs"),
        nthread=10
    )
    tre |>
        subset(is.na(drug2id)) |>
        aggregate(
            assay="sensitivity",
            viability=mean(viability),
            by=c("drug1id", "drug1dose", "cellid")
        ) |>
        aggregate(
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
                    Rsq=as.numeric(unlist(attributes(fit))),
                    auc=auc,
                    ic50=ic50
                )
            },
            by=c("drug1id", "cellid"),
            enlist=FALSE,
            nthread=20
        ) -> mono_profiles
    tre$mono_profiles <- mono_profiles

    # -- extract monotherapy drug observations
    ## FIXME:: This drops the aggregations attribute... Do we need it?
    mono_profiles <- tre$mono_profiles
    ## TODO:: Add helper methods to make finding these columns easier!
    mono_cols <- setdiff(  # identify assay specific columns
        colnames(mono_profiles),
        c(colnames(rowData(tre)), colnames(colData(tre)))
    )
    mono_profiles <- unique(mono_profiles[
        !is.na(HS) & Rsq > 0.75,  # drop failed fits and not passing Rsq test
        mget(setdiff(mono_cols, c("auc", "ic50"))),
        by=c("drug1id", "cellid")
    ])

    # -- build out combination modelling table with dose! Needed for Loewe
    combo_profiles <- tre$sensitivity[
        !is.na(drug2id),
        .(
            mean_viability=mean(viability),
            mean_drug1dose=mean(drug1dose),
            mean_drug2dose=mean(drug2dose)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    # TODO:: Do we want to keep NA values? Add all.x=TRUE if so.
    combo_profiles <- merge.data.table(
        combo_profiles, mono_profiles,
        by=c("drug1id", "cellid")
    )
    combo_profiles <- merge.data.table(
        combo_profiles, mono_profiles,
        by.x=c("drug2id", "cellid"), by.y=c("drug1id", "cellid"),
        suffixes=c("_2", "_1")
    )
    setkeyv(combo_profiles, c("drug1id", "drug2id", "cellid"))
    setcolorder(combo_profiles, c("drug1id", "drug2id", "cellid"))

    # -- compute synergy metrics
    ## Here score ~ combination index
    combo_profiles |>
        aggregate({
            # predict single agent viability for this combo
            v1 <- min(PharmacoGx:::.Hill(
                log10(mean_drug1dose),
                c(HS_1, E_inf_1, log10(EC50_1))
            ) / 100, 1)
            v2 <- min(PharmacoGx:::.Hill(
                log10(mean_drug2dose),
                c(HS_2, E_inf_2, log10(EC50_2))
            ) / 100, 1)
            # alias variables for readable math
            v <- min(mean_viability / 100, 1)
            r1 <- 1 - v1
            r2 <- 1 - v2
            r <- 1 - v
            rmax1 <- 1 - (E_inf_1 / 100)
            rmax2 <- 1 - (E_inf_2 / 100)
            rmin <- 0
            # do math
            HSA <- max(r1, r2)
            HSA_score <- HSA / r
            BLISS <- r1 + r2 - (r1 * r2)
            BLISS_score <- BLISS / r
            LOEWE_1 <- (d1 / EC50_1) * (1 / ((emin1 - v) / (v - emax1)) ^ (1 / HS_1))
            LOEWE_2 <- (d2 / EC50_2) * (1 / ((emin2 - v) / (v - emax2)) ^ (1 / HS_2))
            LOEWE_score <- LOEWE_1 + LOEWE_2
            # return list
            list(r1=r1, d1=mean_drug1dose, d2=mean_drug2dose, r2=r2, r=r, HSA=HSA, HSA_score=HSA_score, BLISS=BLISS,
                BLISS_score=BLISS_score,
                LOEWE_score=LOEWE_score
            )
        },
        by=c("drug1id", "drug2id", "cellid"),
        enlist=FALSE,
        nthread=1
        ) -> prof2

    ## PERCENTGROWTHNOTZ has missing values
    ## compute viability from T_72 / C_72
    viability <- tre[["sensitivity"]][,
            c(idCols(tre), "TESTVALUE", "CONTROLVALUE"),
            with = FALSE
        ]
    ## We have negative TESTVALUE => nagtive viability, meaning
    ## blank absorbance reading is for some reason higher than the sample
    ## either because the measurements are close to machine error value
    ## or the blank was contaminated
    viability[TESTVALUE < 0,]
    ## negative TESTVALUE in a very marginal range, replace them with 0
    viability[TESTVALUE < 0, TESTVALUE := 0]
    ## logLogisticRegression will bark for viability exceeding negative control
    viability[, `:=`(
        viability =  ifelse(
            TESTVALUE > CONTROLVALUE,
            100, ## Cap E_min with 100
            (TESTVALUE / CONTROLVALUE) * 100
            ),
        TESTVALUE = NULL,
        CONTROLVALUE = NULL
    )]
    tre[[".viability"]] <- viability
    mono_response <- viability |> getMonoResponse()
    #mono_params <- qs::qread("/home/lifeifei/data/mono_params.qs")
    mono_params <- mono_response |>
        aggregate2(PharmacoGx::logLogisticRegression(dose, avg_viability),
                   by = c("drugid", "cellid"), enlist = FALSE
    ) ## should run without warnings or errors

    combo_response <- tre[["sensitivity"]][
        !is.na(drug1id) & !is.na(drug2id),
        c(idCols(tre), "TESTVALUE", "CONTROLVALUE"),
        with = FALSE
        ][,
        `:=`(
        viability =  ifelse(
            TESTVALUE > CONTROLVALUE,
            100, ## Cap E_min with 100
            (TESTVALUE / CONTROLVALUE) * 100
            ),
        TESTVALUE = NULL,
        CONTROLVALUE = NULL)
    ][,
        viability := mean(viability),
        by = .(drug1id, drug2id, drug1dose, drug2dose, cellid)
    ][, replicate_id := NULL]

    loewe_input <- combo_response[mono_params,
        on = c(drug1id = "drugid", cellid = "cellid")
    ]
    param_names <- setdiff(colnames(loewe_input), colnames((combo_response)))
    setnames(loewe_input,
             old = param_names,
             new = paste(param_names, "1", sep = "_"))
    loewe_input <- loewe_input[mono_params,
        on = c(drug2id = "drugid", cellid = "cellid")
    ]
    setnames(loewe_input,
             old = param_names,
             new = paste(param_names, "2", sep = "_"))
    loewe_input <- loewe_input[!is.na(viability)]
    effectToDose <- function(E, HS, EC50, E_inf) {
        return(
            EC50 * (100 - E) / (E - E_inf)
        )
    }
    computeLoewe <- function(
        dose1, dose2, HS_1, HS_2, E_inf_1, E_inf_2, EC50_1, EC50_2, E
    ) {
        ## TODO:: this merely computes Loewe combination score
        ## need to find a way to estimate E_loewe
        dose_ratio_1 <- dose1 / effectToDose(E = E, HS = HS_1, E_inf = E_inf_1, EC50 = EC50_1)
        dose_ratio_2 <- dose2 / effectToDose(E = E, HS = HS_2, E_inf = E_inf_2, EC50 = EC50_2)
        CI <- dose_ratio_1 + dose_ratio_2
        return(CI)
    }

    Loewe_CI <- loewe_input |>
        aggregate2(
            Loewe_CI = computeLoewe(
                dose1 = drug1dose,
                dose2 = drug2dose,
                HS_1 = HS_1,
                HS_2 = HS_2,
                E_inf_1 = E_inf_1,
                E_inf_2 = E_inf_2,
                EC50_1 = EC50_1,
                EC50_2 = EC50_2,
                E = viability
            ),
            by = c("drug1id",
                   "drug2id",
                   "cellid",
                   "drug1dose",
                   "drug2dose",
                   "viability")
    )
}