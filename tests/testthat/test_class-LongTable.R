library(testthat)
library(data.table)


# ==== LongTable-acessors.R

# see https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation for
# explanation
test_that("`rowData,LongTable-method` orders data correctly", {

})

# ==== LongTable-utils.R

testthat::test_that("`reindex,LongTable-method` does not corrupt data relationships", {
    nlt <- reindex(lt)
    for (i in seq_along(assayNames(lt))) {
        print(i)
        assay1 <- assay(lt, i, withDimnames=TRUE)
        setkeyv(assay1, idCols(lt))
        assay2 <- assay(nlt, i, withDimnames=TRUE)
        setkeyv(assay2, idCols(lt))
        testthat::expect_true(all.equal(assay1, assay2))
    }
    assayL1 <- assays(lt)
    assayL2 <- assays(nlt)
    for (i in seq_along(assayL1)) {
        print(i)
        testthat::expect_true(all.equal(assayL1[[i]], assayL2[[i]]))
    }
})

testthat::test_that("`reindex,LongTale-method` does not mutate by reference", {
    .lt <- copy(lt)
    nlt <- reindex(lt)
    testthat::expect_true(all.equal(.lt, lt))
})

testthat::test_that("`assay,LongTable-method` and `assays,LongTable-method` return equivalent data", {
    assay_list <- lapply(seq_along(assayNames(lt)), FUN=assay,
        x=lt, withDimnames=TRUE)
    assays_ <- assays(lt)
    for (i in seq_along(assay_list)) {
        print(i)
        testthat::expect_true(all.equal(assay_list[[i]], assays_[[i]]))
    }
})