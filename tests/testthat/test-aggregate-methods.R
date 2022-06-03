library(testthat)
library(CoreGx)
library(BiocParallel)
library(data.table)

data(nci_TRE_small)
tre <- copy(nci_TRE_small)

testthat::test_that("`aggregate2` is equivalent to raw data.table aggregation", {
    sens <- tre$sensitivity
    by <- c("drug1id", "drug2id", "cellid")
    ## Single threaded case
    agg_res <- sens |> aggregate2(
        mean(drug1dose),
        mean(drug2dose),
        mean(viability),
        by=by
    )
    ## Mutlithreaded case (via bplapply)
    agg_res_parallel <- sens |> aggregate2(
        mean(drug1dose),
        mean(drug2dose),
        mean(viability),
        by=by,
        nthread=2
    )
    ## data.table default
    dt_res <- sens[,
        .(
            mean_drug1dose=mean(drug1dose),
            mean_drug2dose=mean(drug2dose),
            mean_viability=mean(viability),
        ),
        by=by
    ]
    expect_true(all.equal(
        agg_res,
        dt_res
    ))
    expect_true(all.equal(
        agg_res_parallel,
        dt_res
    ))
})