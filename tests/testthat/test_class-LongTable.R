library(testthat)
library(data.table)


# ==== LongTable-accessors.R

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

## TODO:: Handle this edge case for both row and column data
#testthat::test_that("`rowData<-`prevent from breaking referential integrity on purpose", {
#    ntre <- copy(tre)
#    rowData_bad <- rowData(ntre)
#    rowData_bad <- rbind(rowData_bad, rowData_bad[.N, ])
#    testthat::expect_error({ rowData(ntre) <- rowData_bad },
#        regexp = ""
#    )
#})

# This warning doesn't trigger if we remove another ID column.
# Instead, an error like in the NCI-ALMANAC script will occur. (Too many duplicate rows)
testthat::test_that("`rowData<-` ensure necessary row ID columns present in the replacement rowData", {
    ntre <- copy(tre)
    rowData_missingID <- rowData(ntre)
    rowIDcols <- rowIDs(ntre)
    rowData_missingID[, rowIDcols[length(rowIDcols)] := NULL] # remove one ID column
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

## TODO:: Add test case for user-modified colIDs

# == @assay slot

testthat::test_that("`assay` invalid assay name and index", {
    testthat::expect_error({ assay(tre, c(1, 2)) },
        regexp = ".*Please specifying a single string assay name or integer index.*"
    )
    testthat::expect_error({ assay(tre, paste(assayNames(tre), collapse = '')) },
        regexp = ".*There is no assay.*"
    )
})

testthat::test_that("`assay<-` invalid assay slot assignment", {
    ntre <- copy(tre)
    testthat::expect_error({ assay(ntre, i = "sensitivity", withDimnames = FALSE) <- NULL },
        regexp = ".*Only a data\\.frame or data\\.table can be assiged to the assay slot!.*"
    ) ## FIXME:: If an integer assay slot index is used for i instead, it can bypass this check
})

testthat::test_that("`assay,LongTable-method` and `assays,LongTable-method` return equivalent data", {
    assay_list <- lapply(seq_along(assayNames(tre)), FUN=assay,
        x=tre, withDimnames=TRUE)
    assays_ <- assays(tre)
    for (i in seq_along(assay_list)) {
        print(i)
        testthat::expect_true(all.equal(assay_list[[i]], assays_[[i]]))
    }
})

testthat::test_that("`assay<-LongTable-method` assignment does not corrupt data relationships", {
    ntre <- copy(tre)
    for (nm in assayNames(tre)) {
        print(nm)
        ntre[[nm]] <- ntre[[nm]]
        testthat::expect_true(all.equal(ntre[[nm]], tre[[nm]]))
        testthat::expect_true(all.equal(assays(ntre, raw=TRUE)[[nm]], assays(tre, raw=TRUE)[[nm]]))
    }
    testthat::expect_true(all.equal(getIntern(ntre)$assayIndex, getIntern(tre)$assayIndex))
})

testthat::test_that("`assay<-LongTable-method` allows non-id column updates", {
    ntre <- copy(tre)
    assay_ <- ntre[["sensitivity"]]
    assay_[, viabilitrey := rnorm(.N)]
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

testthat::test_that("`assay<-LongTable-method` prevents modified row ID in new assay from breaking referential integrity", {
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

testthat::test_that("`assay<-LongTable-method` prevents modified column ID in new assay from breaking referential integrity", {
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

# ==== LongTable-utils.R

# == subset

testthat::test_that("`subset,LongTable-method` works with call queries", {
    ntre <- subset(tre,
        drug1id %in% unique(drug1id)[1:5],
        cellid %in% unique(cellid)[1:5]
    )
    testthat::expect_s4_class(ntre, "LongTable")
    testthat::expect_equal(
        rowData(ntre),
        rowData(tre)[drug1id %in% unique(drug1id)[1:5]]
    )
    testthat::expect_equal(
        colData(ntre),
        colData(tre)[cellid %in% unique(cellid)[1:5]]
    )
    # check for NA values in the key column of the assay
    testthat::expect_true(
        !anyNA(assays(ntre, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    ntre2 <- tre[
        .(drug1id %in% unique(drug1id)[1:5]),
        .(cellid %in% unique(cellid)[1:5])
    ]
    testthat::expect_s4_class(ntre2, "LongTable")
    testthat::expect_equal(
        rowData(ntre2),
        rowData(tre)[drug1id %in% unique(drug1id)[1:5]]
    )
    testthat::expect_equal(
        colData(ntre2),
        colData(tre)[cellid %in% unique(cellid)[1:5]]
    )
    # check for NA values in the key column of the assay
    testthat::expect_true(
        !anyNA(assays(ntre2, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    testthat::expect_equal(ntre, ntre2)
})

testthat::test_that("`subset,LongTable-method` works with regex queries", {
    ntre <- subset(tre,
        c("vemurafenib", "Vismodegib"),
        c("UACC*", "SK-MEL-*")
    )
    testthat::expect_s4_class(ntre, "LongTable")
    testthat::expect_equal(
        rowData(ntre),
        rowData(tre)[grepl("vemurafenib|Vismodegib", rownames(tre)), ]
    )
    testthat::expect_equal(
        colData(ntre),
        colData(tre)[grepl("UACC*|SK-MEL-*", colnames(tre)), ]
    )
    ntre2 <- tre[
        c("vemurafenib", "Vismodegib"),
        c("UACC*", "SK-MEL-*")
    ]
    testthat::expect_s4_class(ntre2, "LongTable")
    testthat::expect_equal(
        rowData(ntre2),
        rowData(tre)[grepl("vemurafenib|Vismodegib", rownames(tre)), ]
    )
    testthat::expect_equal(
        colData(ntre2),
        colData(tre)[grepl("UACC*|SK-MEL-*", colnames(tre)), ]
    )
    testthat::expect_equal(ntre, ntre2)
})

testthat::test_that("`CoreGx:::.subsetByIndex` is equivalent to subsetting the raw data", {
    keepRows <- rowData(tre, key=TRUE)[drug1id %in% drug1id[1:5], ]
    fullAssay <- tre$sensitivity
    rawSubset <- fullAssay[drug1id %in% keepRows$drug1id, ]
    aindex <- mutable(getIntern(tre, "assayIndex"))
    subindex <- aindex[rowKey %in% keepRows$rowKey, ]
    ntre <- CoreGx:::.subsetByIndex(tre, subindex)
    testthat::expect_true(
        !anyNA(assays(ntre, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    assayByIndex <- ntre$sensitivity
    testthat::expect_true(all.equal(rawSubset, assayByIndex))
})

testthat::test_that("`subset,LongTable-method` works with row and column names", {
    ntre <- subset(tre, rownames(tre)[1:5], colnames(tre)[1:5])
    testthat::expect_equal(rownames(ntre), rownames(tre)[1:5])
    testthat::expect_true(all.equal(rowData(ntre), rowData(tre)[1:5, ]))
    testthat::expect_equal(colnames(ntre), colnames(tre)[1:5])
    testthat::expect_true(all.equal(colData(ntre), colData(tre)[1:5, ]))
})

# == reindex

testthat::test_that("`reindex,LongTale-method` does not mutate by reference", {
    .tre <- copy(tre)
    ntre <- reindex(tre)
    testthat::expect_true(all.equal(.tre, tre))
})

testthat::test_that("`reindex,LongTable-method` has same index as LongTable constructor", {
    ntre <- reindex(tre)
    testthat::expect_true(all.equal(getIntern(ntre, "assayIndex"), getIntern(tre, "assayIndex")))
    testthat::expect_true(all.equal(assays(ntre, raw=TRUE), assays(tre, raw=TRUE)))
})

testthat::test_that("`reindex,LongTable-method` does not corrupt data relationships", {
    ntre <- reindex(tre)
    for (i in seq_along(assayNames(tre))) {
        print(i)
        assay1 <- assay(tre, i, withDimnames=TRUE)
        setkeyv(assay1, idCols(tre))
        assay2 <- assay(ntre, i, withDimnames=TRUE)
        setkeyv(assay2, idCols(tre))
        testthat::expect_true(all.equal(assay1, assay2))
    }
    assayL1 <- assays(tre)
    assayL2 <- assays(ntre)
    for (i in seq_along(assayL1)) {
        print(i)
        testthat::expect_true(all.equal(assayL1[[i]], assayL2[[i]]))
    }
})

testthat::test_that("`reindex,LongTable-method` removes gaps in keys in subset LongTable", {
    stre <- tre[seq.int(1, round(dim(tre)[1] * 0.5), by = 2), ] ## subset data
    stre <- reindex(stre)
    ## check if rowData and colData keys have gaps
    row_keys <- rowData(stre, key = TRUE)$rowKey
    col_keys <- colData(stre, key = TRUE)$colKey
    has_no_gaps_in_row <- rle(diff(row_keys))$value == 1
    has_no_gaps_in_col <- rle(diff(col_keys))$value == 1
    testthat::expect_true(has_no_gaps_in_row)
    testthat::expect_true(has_no_gaps_in_col)
    ## check if assays' keys have gaps
#    for (i in seq_along(assayNames(stre))) {
#        assay_name <- assayNames(stre)[i]
#        assay_keys <- assay(stre, i, key = FALSE, withDimnames = FALSE)[[assay_name]]
#        has_no_gaps_in_assay <- rle(diff(assay_keys))$value == 1
#        if (length(has_no_gaps_in_assay) > 1) print(i)
#        testthat::expect_true(has_no_gaps_in_assay)
#    } # In summary assay, the repeating primary key failing this test.
})

# == [[

testthat::test_that("`[[,LongTable-method` returns assay metadata always with dimnames",{
    testthat::expect_warning({ tre[[1, withDimnames = FALSE, metadata = TRUE]] },
        regexp = ".*Unable to return metadata without dimnames, proceeding as if withDimnames=TRUE.*"
    )
})

testthat::test_that("`[[,LongTable-method` when keys = TRUE, ignore withDimnames and metadata",{
    testthat::expect_warning({ tre[[1, keys = TRUE]] },
        regexp = ".*Ignoring withDimnames and metadata arguments when keys=TRUE.*"
    )
})

testthat::test_that("`[[,LongTable-method` allows only one assay selection at a time",{
    testthat::expect_error({ tre[[1:2]] },
        regexp = ".*Please only select one assay.*"
    )
})