library(testthat)
library(data.table)


# ==== LongTable-accessors.R

# see https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation for
# explanation
test_that("`rowData,LongTable-method` orders data correctly", {
    
})

# == @rowData slot

testthat::test_that("`rowData<-` rowData must be updated with data.table or data.frame", {
    nlt <- copy(lt)
    testthat::expect_error({ rowData(nlt) <- NULL }, ## rowData slot
        regexp = ".*Please pass a data\\.frame or data\\.table to update the rowData slot.*"
    )
})

## TODO:: Handle this edge case for both row and column data
#testthat::test_that("`rowData<-`prevent from breaking referential integrity on purpose", {
#    nlt <- copy(lt)
#    rowData_bad <- rowData(nlt)
#    rowData_bad <- rbind(rowData_bad, rowData_bad[.N, ])
#    testthat::expect_error({ rowData(nlt) <- rowData_bad },
#        regexp = ""
#    )
#})

# This warning doesn't trigger if we remove another ID column.
# Instead, an error like in the NCI-ALMANAC script will occur. (Too many duplicate rows)
testthat::test_that("`rowData<-` ensure necessary row ID columns present in the replacement rowData", {
    nlt <- copy(lt)
    rowData_missingID <- rowData(nlt)
    rowIDcols <- rowIDs(nlt)
    rowData_missingID[, rowIDcols[length(rowIDcols)] := NULL] # remove one ID column
    testthat::expect_warning({ rowData(nlt) <- rowData_missingID },
        regexp = ".*The function will attempt to join with existing rowIDs, but this may fail!.*"
    )
})

# == @colData slot

testthat::test_that("`colData<-` colData must be updated with data.table or data.frame", {
    nlt <- copy(lt)
    testthat::expect_error({ colData(nlt) <- NULL }, ## colData slot
        regexp = ".*Please pass a data\\.frame or data\\.table.*"
    )
})

## TODO:: Add test case for user-modified colIDs

# == @assay slot

testthat::test_that("`assay` invalid assay name and index", {
    testthat::expect_error({ assay(lt, c(1, 2)) },
        regexp = ".*Please specifying a single string assay name or integer index.*"
    )
    testthat::expect_error({ assay(lt, paste(assayNames(lt), collapse = '')) },
        regexp = ".*There is no assay.*"
    )
})

testthat::test_that("`assay<-` invalid assay slot assignment", {
    nlt <- copy(lt)
    testthat::expect_error({ assay(nlt, i = "sensitivity", withDimnames = FALSE) <- NULL },
        regexp = ".*Only a data\\.frame or data\\.table can be assiged to the assay slot!.*"
    ) ## FIXME:: If an integer assay slot index is used for i instead, it can bypass this check
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

testthat::test_that("`assay<-LongTable-method` assignment does not corrupt data relationships", {
    nlt <- copy(lt)
    for (nm in assayNames(lt)) {
        print(nm)
        nlt[[nm]] <- nlt[[nm]]
        testthat::expect_true(all.equal(nlt[[nm]], lt[[nm]]))
        testthat::expect_true(all.equal(assays(nlt, raw=TRUE)[[nm]], assays(lt, raw=TRUE)[[nm]]))
    }
    testthat::expect_true(all.equal(getIntern(nlt)$assayIndex, getIntern(lt)$assayIndex))
})

testthat::test_that("`assay<-LongTable-method` allows non-id column updates", {
    nlt <- copy(lt)
    assay_ <- nlt[["sensitivity"]]
    assay_[, viabililty := rnorm(.N)]
    nlt[["sensitivity"]] <- assay_
    testthat::expect_true(all.equal(nlt[["sensitivity"]], assay_))
    testthat::expect_false(isTRUE(all.equal(nlt[["sensitivity"]], lt[["sensitivity"]])))
})

testthat::test_that("`assay<-LongTable-method` prevents id column updates", {
    nlt <- copy(lt)
    assay_ <- nlt[["sensitivity"]]
    assay_[, drug1dose := rnorm(.N)]
    testthat::expect_error({ nlt[["sensitivity"]] <- assay_ },
        regexp=".*Identifier columns cannot be modified via assay assignment!.*"
    )
    testthat::expect_true(all.equal(nlt$sensitivity, lt$sensitivity))
})

testthat::test_that("`assay<-LongTable-method` allows simple summary assignments", {
    nlt <- copy(lt)
    sens <- nlt$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    testthat::expect_silent(nlt$sens_sum <- sens_sum)
    testthat::expect_true(all.equal(
        rowIDs(lt, data=TRUE),
        unique(nlt$sens_sum[, rowIDs(nlt), with=FALSE]),
        check.attributes=FALSE
    ))
    testthat::expect_true(all.equal(
        colIDs(lt, data=TRUE),
        unique(nlt$sens_sum[, colIDs(nlt), with=FALSE])[order(mget(colIDs(nlt)))],
        check.attributes=FALSE
    ))
})

testthat::test_that("`assay<-LongTable-method` summary assignment doesn't break referential integrity", {
    nlt <- copy(lt)
    sens <- nlt$sensitivity
    sens_sum <- sens[,
        .(
            mean_drug1dose=mean(drug1dose, na.rm=TRUE),
            mean_drug2dose=mean(drug2dose, na.rm=TRUE),
            mean_viability=mean(viability, na.rm=TRUE)
        ),
        by=.(drug1id, drug2id, cellid)
    ]
    testthat::expect_silent(nlt$sens_sum <- sens_sum)
    testthat::expect_true(all.equal(rowData(lt), rowData(nlt)))
    testthat::expect_true(all.equal(colData(lt), colData(nlt)))
    non_summary_assays <- setdiff(assayNames(nlt), "sens_sum")
    for (aname in non_summary_assays) {
        testthat::expect_true(all.equal(
            lt[[aname]],
            nlt[[aname]]
        ))
    }
})

testthat::test_that("`assay<-LongTable-method` prevents modified row ID in new assay from breaking referential integrity", {
    nlt <- copy(lt)
    sens <- nlt$sensitivity
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
    testthat::expect_error({ nlt$sens_sum <- sens_sum },
        regexp = paste(
            ".*One or more rowIDs\\(x\\) columns have been modified.",
            "Identifier columns cannot be modified via assay assignment!.*"
            )
    )
})

testthat::test_that("`assay<-LongTable-method` prevents modified column ID in new assay from breaking referential integrity", {
    nlt <- copy(lt)
    sens <- nlt$sensitivity
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
    testthat::expect_error({ nlt$sens_sum <- sens_sum },
        regexp = paste(
            ".*One or more colIDs\\(x\\) column have been modified.",
            "Identifier columns cannot be modified via assay assignment!.*"
            )
    )
})

# == @metadata slot

testthat::test_that("`metadata<-` invalid metadata slot assignment", {
    nlt <- copy(lt)
    testthat::expect_error({ metadata(nlt) <- NULL },
        regexp = ".*The `metadata` slot must be a list!.*"
    )
})

# ==== LongTable-utils.R

# == subset

testthat::test_that("`subset,LongTable-method` works with call queries", {
    nlt <- subset(lt,
        drug1id %in% unique(drug1id)[1:5],
        cellid %in% unique(cellid)[1:5]
    )
    testthat::expect_s4_class(nlt, "LongTable")
    testthat::expect_equal(
        rowData(nlt),
        rowData(lt)[drug1id %in% unique(drug1id)[1:5]]
    )
    testthat::expect_equal(
        colData(nlt),
        colData(lt)[cellid %in% unique(cellid)[1:5]]
    )
    # check for NA values in the key column of the assay
    testthat::expect_true(
        !anyNA(assays(nlt, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    nlt2 <- lt[
        .(drug1id %in% unique(drug1id)[1:5]),
        .(cellid %in% unique(cellid)[1:5])
    ]
    testthat::expect_s4_class(nlt2, "LongTable")
    testthat::expect_equal(
        rowData(nlt2),
        rowData(lt)[drug1id %in% unique(drug1id)[1:5]]
    )
    testthat::expect_equal(
        colData(nlt2),
        colData(lt)[cellid %in% unique(cellid)[1:5]]
    )
    # check for NA values in the key column of the assay
    testthat::expect_true(
        !anyNA(assays(nlt2, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    testthat::expect_equal(nlt, nlt2)
})

testthat::test_that("`subset,LongTable-method` works with regex queries", {
    nlt <- subset(lt,
        c("vemurafenib", "Vismodegib"),
        c("UACC*", "SK-MEL-*")
    )
    testthat::expect_s4_class(nlt, "LongTable")
    testthat::expect_equal(
        rowData(nlt),
        rowData(lt)[grepl("vemurafenib|Vismodegib", rownames(lt)), ]
    )
    testthat::expect_equal(
        colData(nlt),
        colData(lt)[grepl("UACC*|SK-MEL-*", colnames(lt)), ]
    )
    nlt2 <- lt[
        c("vemurafenib", "Vismodegib"),
        c("UACC*", "SK-MEL-*")
    ]
    testthat::expect_s4_class(nlt2, "LongTable")
    testthat::expect_equal(
        rowData(nlt2),
        rowData(lt)[grepl("vemurafenib|Vismodegib", rownames(lt)), ]
    )
    testthat::expect_equal(
        colData(nlt2),
        colData(lt)[grepl("UACC*|SK-MEL-*", colnames(lt)), ]
    )
    testthat::expect_equal(nlt, nlt2)
})

testthat::test_that("`CoreGx:::.subsetByIndex` is equivalent to subsetting the raw data", {
    keepRows <- rowData(lt, key=TRUE)[drug1id %in% drug1id[1:5], ]
    fullAssay <- lt$sensitivity
    rawSubset <- fullAssay[drug1id %in% keepRows$drug1id, ]
    aindex <- mutable(getIntern(lt, "assayIndex"))
    subindex <- aindex[rowKey %in% keepRows$rowKey, ]
    nlt <- CoreGx:::.subsetByIndex(lt, subindex)
    testthat::expect_true(
        !anyNA(assays(nlt, raw=TRUE)[["sensitivity"]]$sensitivity)
    )
    assayByIndex <- nlt$sensitivity
    testthat::expect_true(all.equal(rawSubset, assayByIndex))
})

testthat::test_that("`subset,LongTable-method` works with row and column names", {
    nlt <- subset(lt, rownames(lt)[1:5], colnames(lt)[1:5])
    testthat::expect_equal(rownames(nlt), rownames(lt)[1:5])
    testthat::expect_true(all.equal(rowData(nlt), rowData(lt)[1:5, ]))
    testthat::expect_equal(colnames(nlt), colnames(lt)[1:5])
    testthat::expect_true(all.equal(colData(nlt), colData(lt)[1:5, ]))
})

# == reindex

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

testthat::test_that("`reindex,LongTable-method` removes gaps in keys in subset LongTable", {
    slt <- lt[seq.int(1, round(dim(lt)[1] * 0.5), by = 2), ] ## subset data
    slt <- reindex(slt)
    ## check if rowData and colData keys have gaps
    row_keys <- rowData(slt, key = TRUE)$rowKey
    col_keys <- colData(slt, key = TRUE)$colKey
    has_no_gaps_in_row <- rle(diff(row_keys))$value == 1
    has_no_gaps_in_col <- rle(diff(col_keys))$value == 1
    testthat::expect_true(has_no_gaps_in_row)
    testthat::expect_true(has_no_gaps_in_col)
    ## check if assays' keys have gaps
#    for (i in seq_along(assayNames(slt))) {
#        assay_name <- assayNames(slt)[i]
#        assay_keys <- assay(slt, i, key = FALSE, withDimnames = FALSE)[[assay_name]]
#        has_no_gaps_in_assay <- rle(diff(assay_keys))$value == 1
#        if (length(has_no_gaps_in_assay) > 1) print(i)
#        testthat::expect_true(has_no_gaps_in_assay)
#    } # In summary assay, the repeating primary key failing this test.
})

# == [[

testthat::test_that("`[[,LongTable-method` returns assay metadata always with dimnames",{
    testthat::expect_warning({ lt[[1, withDimnames = FALSE, metadata = TRUE]] },
        regexp = ".*Unable to return metadata without dimnames, proceeding as if withDimnames=TRUE.*"
    )
})

testthat::test_that("`[[,LongTable-method` when keys = TRUE, ignore withDimnames and metadata",{
    testthat::expect_warning({ lt[[1, keys = TRUE]] },
        regexp = ".*Ignoring withDimnames and metadata arguments when keys=TRUE.*"
    )
})

testthat::test_that("`[[,LongTable-method` allows only one assay selection at a time",{
    testthat::expect_error({ lt[[1:2]] },
        regexp = ".*Please only select one assay.*"
    )
})