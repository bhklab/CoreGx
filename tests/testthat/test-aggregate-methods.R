library(testthat)
library(CoreGx)
library(BiocParallel)
library(data.table)

data(nci_TRE_small)
tre <- copy(nci_TRE_small)
by <- c("treatment1id", "treatment2id", "sampleid")
sens <- tre$sensitivity


## -- Computing aggregations


testthat::test_that("`aggregate2` is equivalent to raw data.table aggregation", {
    ## Single threaded case
    agg_res <- sens |>
        subset(treatment1id %in% unique(treatment1id)[1:3]) |>
        aggregate2(
            mean(treatment1dose), mean(treatment2dose), mean(viability),
            by=by
        )
    ## Mutlithreaded case (via bplapply)
    agg_res_parallel <- sens |>
        subset(treatment1id %in% unique(treatment1id)[1:3]) |>
        aggregate2(
            mean(treatment1dose), mean(treatment2dose), mean(viability),
            by=by,
            nthread=2
        )
    ## data.table default
    dt_res <- sens[
        treatment1id %in% unique(treatment1id)[1:3],
        .(
            mean_treatment1dose=mean(treatment1dose),
            mean_treatment2dose=mean(treatment2dose),
            mean_viability=mean(viability)
        ),
        by=by
    ]
    expect_true(all.equal(
        agg_res,
        dt_res,
        check.attributes=FALSE  # to allow addition of aggregate call as attribute
    ))
    expect_true(all.equal(
        agg_res_parallel,
        dt_res,
        check.attributes=FALSE
    ))
})

testthat::test_that("`aggregate,LongTable-method` is equivalent to aggregating the raw assay data.table", {
    agg_tre <- tre |>
        aggregate(
            assay="sensitivity",
            mean_viability=mean(viability),
            by=by
        )
    agg_tre_parallel <- tre |>
        subset(treatment1id %in% unique(treatment1id)[1:3]) |>
        aggregate(
            assay="sensitivity",
            mean_viability=mean(viability),
            by=by,
            nthread=2
        )
    agg_dt <- tre$sensitivity[,
        .(mean_viability=mean(viability)),
        by=by
    ]
    agg_dt_small <- tre$sensitivity[
        treatment1id %in% unique(treatment1id)[1:3],
        .(mean_viability=mean(viability)),
        by=by
    ]
    expect_true(all.equal(
        agg_tre, agg_dt,
        check.attributes=FALSE
    ))
    expect_true(all.equal(
        agg_tre_parallel, agg_dt_small,
        check.attributes=FALSE
    ))
})

testthat::test_that("`aggregate2` and `aggregate,LongTable-method` automatic naming works correctly", {
    ## aggregate2
    agg2_named <- sens |>
        aggregate2(
            mean_viability=mean(viability), mean_treatment1dose=mean(treatment1dose),
                mean_treatment2dose=mean(treatment2dose),
            by=by
        )
    agg2_unnamed <- sens |>
        aggregate2(
            mean(viability), mean(treatment1dose), mean(treatment2dose),
            by=by
        )
    testthat::expect_true(all.equal(
        agg2_named, agg2_unnamed,
        check.attributes=FALSE
    ))
    ## aggregate,LongTable-method
    agg_named <- tre |>
        aggregate(
            "sensitivity",
            mean_viability=mean(viability), mean_treatment1dose=mean(treatment1dose),
                mean_treatment2dose=mean(treatment2dose),
            by=by
        )
    agg_unnamed <- tre |>
        aggregate(
            "sensitivity",
            mean(viability), mean(treatment1dose), mean(treatment2dose),
            by=by
        )
    testthat::expect_true(all.equal(
        agg_named, agg_unnamed,
        check.attributes=FALSE
    ))
})


## -- Assigning aggregated assays

testthat::test_that("`Assignment doesn't modify summarized assay data", {
    ntre <- copy(tre)
    sens_summary <- tre |>
        aggregate(
            "sensitivity",
            mean_viability=mean(viability, na.rm=TRUE),
            mean_treatment1dose=mean(treatment1dose, na.rm=TRUE),
            mean_treatment2dose=mean(treatment2dose, na.rm=TRUE),
            by=by
        )
    ntre$sens_summary <- sens_summary
    sens_summary2 <- unique(ntre$sens_summary[, .SD, .SDcols=colnames(sens_summary)])
    setkeyv(sens_summary, by)
    setkeyv(sens_summary2, by)
    testthat::expect_true(all.equal(
        sens_summary,
        sens_summary2,
        check.attributes=FALSE
    ))
})

testthat::test_that("`reindex,LongTable-method` doesn't corrupt referrential integrity of summarized assays", {
    ntre <- copy(tre)
    sens_summary <- tre |>
        aggregate(
            "sensitivity",
            mean_viability=mean(viability), mean_treatment1dose=mean(treatment1dose),
                mean_treatment2dose=mean(treatment2dose),
            by=by
        )
    ntre$sens_summary <- sens_summary
    ntre2 <- reindex(ntre)
    expect_true(all.equal(
        ntre$sens_summary,
        ntre2$sens_summary
    ))
})

testthat::test_that("`subset,LongTable-method` works correctly with summary assays", {
    ntre <- copy(tre)
    sens_summary <- tre |>
        aggregate(
            "sensitivity",
            mean_viability=mean(viability), mean_treatment1dose=mean(treatment1dose),
                mean_treatment2dose=mean(treatment2dose),
            by=by
        )
    ntre$sens_summary <- sens_summary
    stre <- subset(ntre, treatment1id %in% treatment1id[1:5])
    testthat::expect_true(CoreGx:::.table_is_subset(
        stre$sens_summary,
        ntre$sens_summary[treatment1id %in% treatment1id[1:5]]
    ))
})