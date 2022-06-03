library(testthat)
library(CoreGx)
library(BiocParallel)
library(data.table)

data(nci_TRE_small)
tre <- copy(nci_TRE_small)
by <- c("drug1id", "drug2id", "cellid")
sens <- tre$sensitivity


## -- Computing aggregations


testthat::test_that("`aggregate2` is equivalent to raw data.table aggregation", {
    ## Single threaded case
    agg_res <- sens |> 
        subset(drug1id %in% unique(drug1id)[1:3]) |>
        aggregate2(
            mean(drug1dose), mean(drug2dose), mean(viability),
            by=by
        )
    ## Mutlithreaded case (via bplapply)
    agg_res_parallel <- sens |> 
        subset(drug1id %in% unique(drug1id)[1:3]) |>
        aggregate2(
            mean(drug1dose), mean(drug2dose), mean(viability),
            by=by,
            nthread=2
        )
    ## data.table default
    dt_res <- sens[
        drug1id %in% unique(drug1id)[1:3],
        .(
            mean_drug1dose=mean(drug1dose),
            mean_drug2dose=mean(drug2dose),
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
        aggregate("sensitivity",
            mean_viability=mean(viability),
            by=by
        )
    agg_tre_parallel <- tre |>
        subset(drug1id %in% unique(drug1id)[1:3]) |>
        aggregate("sensitivity",
            mean_viability=mean(viability),
            by=by,
            nthread=2
        )
    agg_dt <- tre$sensitivity[,
        .(mean_viability=mean(viability)),
        by=by
    ]
    agg_dt_small <- tre$sensitivity[
        drug1id %in% unique(drug1id)[1:3],
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
            mean_viability=mean(viability), mean_drug1dose=mean(drug1dose),
                mean_drug2dose=mean(drug2dose),
            by=by
        )
    agg2_unnamed <- sens |>
        aggregate2(
            mean(viability), mean(drug1dose), mean(drug2dose),
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
            mean_viability=mean(viability), mean_drug1dose=mean(drug1dose),
                mean_drug2dose=mean(drug2dose),
            by=by
        )
    agg_unnamed <- tre |>
        aggregate(
            "sensitivity",
            mean(viability), mean(drug1dose), mean(drug2dose),
            by=by
        )
    testthat::expect_true(all.equal(
        agg_named, agg_unnamed,
        check.attributes=FALSE
    ))
})


## -- Assigning aggregated assays