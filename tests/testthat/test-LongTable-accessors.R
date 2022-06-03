library(testthat)
library(CoreGx)
library(data.table)

data(nci_TRE_small)
tre <- nci_TRE_small

## tre = TreatmentResponseExperiment
## ntre = New TreatmentResponseExperiment

# see https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation for
# explanation
test_that("`rowData,LongTable-method` orders data correctly", {
    ## TODO::
})

# == @rowData slot

testthat::test_that("`rowData<-` rowData must be updated with data.table or data.frame", {
    ntre <- copy(tre)
    testthat::expect_error({ rowData(ntre) <- NULL }, ## rowData slot
        regexp = ".*Please pass a data.frame or data.table to update\nthe rowData slot.*"
    )
})


testthat::test_that("`rowData<-` prevents intentionally breaking referential integrity", {
    ntre <- copy(tre)
    rowData_bad <- rowData(ntre)
    rowData_bad <- rbind(rowData_bad, rowData_bad[.N, ])
    testthat::expect_warning({ rowData(ntre) <- rowData_bad },
        regexp = ".*The ID columns are duplicated for rows [0-9]+! These rows will be dropped before assignment."
    )
})

testthat::test_that("`colData<-` prevents intentionally breaking referential integrity", {
    ntre <- copy(tre)
    colData_bad <- colData(ntre)
    colData_bad <- rbind(colData_bad, colData_bad[.N, ])
    testthat::expect_warning({ colData(ntre) <- colData_bad },
        regexp =  ".*The ID columns are duplicated for rows [0-9]+! These rows will be dropped before assignment."
    )
})

# This warning doesn't trigger if we remove another ID column.
# Instead, an error like in the NCI-ALMANAC script will occur. (Too many duplicate rows)
testthat::test_that("`rowData<-` ensures necessary row ID columns present in the replacement rowData", {
    ntre <- copy(tre)
    rowData_missingID <- rowData(ntre)
    rowData_missingID[, (rowIDs(ntre)[1]) := NULL] # remove one ID column
    testthat::expect_warning({ rowData(ntre) <- rowData_missingID },
        regexp = ".*The function will attempt to join with existing rowIDs, but this may fail!.*"
    )
})

# == @colData slot

testthat::test_that("`colData<-` colData must be updated with data.table or data.frame", {
    ntre <- copy(tre)
    testthat::expect_error({ colData(ntre) <- NULL }, ## colData slot
        regexp = ".*Please pass a data\\.frame or data\\.table.*"
    )
})

# == @assay slot

testthat::test_that("`assay` invalid assay name and index", {
    testthat::expect_error({ assay(tre, c(1, 2)) },
        regexp = ".*Please specifying a single string assay name or integer.*"
    )
    testthat::expect_error({ assay(tre, paste(assayNames(tre), collapse = '')) },
        regexp = ".*There is no assay.*"
    )
})

testthat::test_that("`assay<-,LongTable-meothd` prevents invalid assay slot assignment", {
    ntre <- copy(tre)
    testthat::expect_error({ assay(ntre, i = "sensitivity", withDimnames = FALSE) <- NULL },
        regexp = ".*Only a\ndata.frame or data.table can be assiged to the assay slot!.*"
    ) ## FIXME:: If an integer assay slot index is used for i instead, it can bypass this check
})

testthat::test_that("`assay,LongTable-method` and `assays,LongTable-method` return equivalent data", {
    assay_list <- lapply(seq_along(assayNames(tre)), FUN=assay,
        x=tre, withDimnames=TRUE)
    assays_ <- assays(tre)
    for (i in seq_along(assay_list)) {
        testthat::expect_true(all.equal(assay_list[[i]], assays_[[i]]))
    }
})

testthat::test_that("`assay<-,LongTable-method` assignment does not corrupt data relationships", {
    ntre <- copy(tre)
    for (nm in assayNames(tre)) {
        ntre[[nm]] <- ntre[[nm]]
        testthat::expect_true(all.equal(ntre[[nm]], tre[[nm]]))
        testthat::expect_true(all.equal(assays(ntre, raw=TRUE)[[nm]], assays(tre, raw=TRUE)[[nm]]))
    }
    testthat::expect_true(all.equal(getIntern(ntre)$assayIndex, getIntern(tre)$assayIndex))
})

testthat::test_that("`assay<-LongTable-method` allows non-id column updates", {
    ntre <- copy(tre)
    assay_ <- ntre[["sensitivity"]]
    assay_[, viability := rnorm(.N)]
    ntre[["sensitivity"]] <- assay_
    testthat::expect_true(all.equal(ntre[["sensitivity"]], assay_))
    testthat::expect_false(isTRUE(all.equal(ntre[["sensitivity"]], tre[["sensitivity"]])))
})

testthat::test_that("`assay<-LongTable-method` prevents id column updates", {
    ntre <- copy(tre)
    assay_ <- ntre[["sensitivity"]]
    assay_[, drug1dose := rnorm(.N)]
    testthat::expect_error({ ntre[["sensitivity"]] <- assay_ },
        regexp=".*Identifier columns cannot be modified via assay assignment!.*"
    )
    testthat::expect_true(all.equal(ntre$sensitivity, tre$sensitivity))
})

testthat::test_that("`assay<-LongTable-method` allows simple summary assignments", {
    ntre <- copy(tre)
    sens <- ntre$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    testthat::expect_silent(ntre$sens_sum <- sens_sum)
    testthat::expect_true(all.equal(
        rowIDs(tre, data=TRUE),
        unique(ntre$sens_sum[, rowIDs(ntre), with=FALSE]),
        check.attributes=FALSE
    ))
    testthat::expect_true(all.equal(
        colIDs(tre, data=TRUE),
        unique(ntre$sens_sum[, colIDs(ntre), with=FALSE])[order(mget(colIDs(ntre)))],
        check.attributes=FALSE
    ))
})

testthat::test_that("`assay<-LongTable-method` summary assignment doesn't break referential integrity", {
    ntre <- copy(tre)
    sens <- ntre$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    testthat::expect_silent(ntre$sens_sum <- sens_sum)
    testthat::expect_true(all.equal(rowData(tre), rowData(ntre)))
    testthat::expect_true(all.equal(colData(tre), colData(ntre)))
    non_summary_assays <- setdiff(assayNames(ntre), "sens_sum")
    for (aname in non_summary_assays) {
        testthat::expect_true(all.equal(
            tre[[aname]],
            ntre[[aname]]
        ))
    }
})

testthat::test_that("`assay<-,LongTable-method` prevents modified row ID in new assay from breaking referential integrity", {
    ntre <- copy(tre)
    sens <- ntre$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    set(sens_sum,
        i     = which(sens_sum[["drug1id"]] == sens_sum[1, drug1id]),
        j     = "drug1id",           # rowID to modify
        value = sens_sum[1, cellid]) # Replace a drug id with a cell id
    testthat::expect_error({ ntre$sens_sum <- sens_sum },
        regexp = paste(
            ".*One or more rowIDs\\(x\\) columns have been modified.",
            "Identifier columns cannot be modified via assay assignment!.*"
            )
    )
})

testthat::test_that("`assay<-,LongTable-method` prevents modified column ID in new assay from breaking referential integrity", {
    ntre <- copy(tre)
    sens <- ntre$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    set(sens_sum,
        i     = which(sens_sum[["cellid"]] == sens_sum[1, cellid]),
        j     = "cellid",             # column ID to modify
        value = sens_sum[1, drug2id]) # Replace a cell ID with a drug ID
    testthat::expect_error({ ntre$sens_sum <- sens_sum },
        regexp = paste(
            ".*One or more colIDs\\(x\\) column have been modified.",
            "Identifier columns cannot be modified via assay assignment!.*"
            )
    )
})

# == @metadata slot

testthat::test_that("`metadata<-` invalid metadata slot assignment", {
    ntre <- copy(tre)
    testthat::expect_error({ metadata(ntre) <- NULL },
        regexp = ".*The `metadata` slot must be a list!.*"
    )
})
