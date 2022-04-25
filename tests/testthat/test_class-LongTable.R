library(testthat)
library(data.table)


# ==== LongTable-acessors.R

# see https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation for
# explanation
test_that("`rowData,LongTable-method` orders data correctly", {

})

# === @assay slot

testthat::test_that("`assay,LongTable-method` and `assays,LongTable-method` return equivalent data", {
    assay_list <- lapply(seq_along(assayNames(lt)), FUN=assay,
        x=lt, withDimnames=TRUE)
    assays_ <- assays(lt)
    for (i in seq_along(assay_list)) {
        print(i)
        testthat::expect_true(all.equal(assay_list[[i]], assays_[[i]]))
    }
})

testthat::test_that("`assay<-LongTable-method` assignment does not corrupt data relationships", {
    nlt <- copy(lt)
    for (nm in assayNames(lt)) {
        print(nm)
        nlt[[nm]] <- nlt[[nm]]
        testthat::expect_true(all.equal(nlt[[nm]], lt[[nm]]))
        testthat::expect_true(all.equal(assays(nlt, raw=TRUE)[[nm]], assays(lt, raw=TRUE)[[nm]]))
        testthat::expect_true(all.equal(getIntern(nlt)$assayIndex, getIntern(lt)$assayIndex))
    }
})

testthat::test_that("`assay<-LongTable-method` allows non-id column updates", {
    nlt <- copy(lt)
    assay_ <- nlt[["sensitivity"]]
    assay_[, viabililty := rnorm(.N)]
    nlt[["sensitivity"]] <- assay_
    testthat::expect_true(all.equal(nlt[["sensitivity"]], assay_))
    testthat::expect_true(is.character(all.equal(nlt[["sensitivity"]], lt[["sensitivity"]])))
})

testthat::test_that("`assay<-LongTable-method` prevents id column updates", {
    nlt <- copy(lt)
})

# ==== LongTable-utils.R

testthat::test_that("`reindex,LongTale-method` does not mutate by reference", {
    .lt <- copy(lt)
    nlt <- reindex(lt)
    testthat::expect_true(all.equal(.lt, lt))
})

testthat::test_that("`reindex,LongTable-method` has same index as LongTable constructor", {
    nlt <- reindex(lt)
    testthat::expect_true(all.equal(getIntern(nlt, "assayIndex"), getIntern(lt, "assayIndex")))
    testthat::expect_true(all.equal(assays(nlt, raw=TRUE), assays(lt, raw=TRUE)))
})

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
