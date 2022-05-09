# library(testthat)
# library(data.table)


# # ==== LongTable-accessors.R

# # see https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation for
# # explanation
# test_that("`rowData,LongTable-method` orders data correctly", {

# })

# # == @assay slot

# testthat::test_that("`assay,LongTable-method` and `assays,LongTable-method` return equivalent data", {
#     assay_list <- lapply(seq_along(assayNames(lt)), FUN=assay,
#         x=lt, withDimnames=TRUE)
#     assays_ <- assays(lt)
#     for (i in seq_along(assay_list)) {
#         print(i)
#         testthat::expect_true(all.equal(assay_list[[i]], assays_[[i]]))
#     }
# })

# testthat::test_that("`assay<-LongTable-method` assignment does not corrupt data relationships", {
#     nlt <- copy(lt)
#     for (nm in assayNames(lt)) {
#         print(nm)
#         nlt[[nm]] <- nlt[[nm]]
#         testthat::expect_true(all.equal(nlt[[nm]], lt[[nm]]))
#         testthat::expect_true(all.equal(assays(nlt, raw=TRUE)[[nm]], assays(lt, raw=TRUE)[[nm]]))
#     }
#     testthat::expect_true(all.equal(getIntern(nlt)$assayIndex, getIntern(lt)$assayIndex))
# })

# testthat::test_that("`assay<-LongTable-method` allows non-id column updates", {
#     nlt <- copy(lt)
#     assay_ <- nlt[["sensitivity"]]
#     assay_[, viabililty := rnorm(.N)]
#     nlt[["sensitivity"]] <- assay_
#     testthat::expect_true(all.equal(nlt[["sensitivity"]], assay_))
#     testthat::expect_false(isTRUE(all.equal(nlt[["sensitivity"]], lt[["sensitivity"]])))
# })

# testthat::test_that("`assay<-LongTable-method` prevents id column updates", {
#     nlt <- copy(lt)
#     assay_ <- nlt[["sensitivity"]]
#     assay_[, drug1dose := rnorm(.N)]
#     testthat::expect_error({ nlt[["sensitivity"]] <- assay_ },
#         regexp=".*Identifier columns cannot be modified via assay assignment!.*"
#     )
#     testthat::expect_true(all.equal(nlt$sensitivity, lt$sensitivity))
# })


# testthat::test_that("`assay<-LongTable-method` allows simple summary assignments", {
#     nlt <- copy(lt)
#     sens <- nlt$sensitivity
#     sens_sum <- sens[,
#         .(
#             mean_drug1dose=mean(drug1dose, na.rm=TRUE),
#             mean_drug2dose=mean(drug2dose, na.rm=TRUE),
#             mean_viability=mean(viability, na.rm=TRUE)
#         ),
#         by=.(drug1id, drug2id, cellid)
#     ]
#     testthat::expect_silent(nlt$sens_sum <- sens_sum)
#     testthat::expect_true(all.equal(
#         rowIDs(lt, data=TRUE),
#         unique(nlt$sens_sum[, rowIDs(nlt), with=FALSE]),
#         check.attributes=FALSE
#     ))
#     testthat::expect_true(all.equal(
#         colIDs(lt, data=TRUE),
#         unique(nlt$sens_sum[, colIDs(nlt), with=FALSE])[order(mget(colIDs(nlt)))],
#         check.attributes=FALSE
#     ))
# })

# testthat::test_that("`assay<-LongTable-method` summary assignment doesn't break referential integrity", {
#     nlt <- copy(lt)
#     sens <- nlt$sensitivity
#     sens_sum <- sens[,
#         .(
#             mean_drug1dose=mean(drug1dose, na.rm=TRUE),
#             mean_drug2dose=mean(drug2dose, na.rm=TRUE),
#             mean_viability=mean(viability, na.rm=TRUE)
#         ),
#         by=.(drug1id, drug2id, cellid)
#     ]
#     testthat::expect_silent(nlt$sens_sum <- sens_sum)
#     testthat::expect_true(all.equal(rowData(lt), rowData(nlt)))
#     testthat::expect_true(all.equal(colData(lt), colData(nlt)))
#     non_summary_assays <- setdiff(assayNames(nlt), "sens_sum")
#     for (aname in non_summary_assays) {
#         testthat::expect_true(all.equal(
#             lt[[aname]],
#             nlt[[aname]]
#         ))
#     }
# })




# # ==== LongTable-utils.R

# testthat::test_that("`reindex,LongTale-method` does not mutate by reference", {
#     .lt <- copy(lt)
#     nlt <- reindex(lt)
#     testthat::expect_true(all.equal(.lt, lt))
# })

# testthat::test_that("`reindex,LongTable-method` has same index as LongTable constructor", {
#     nlt <- reindex(lt)
#     testthat::expect_true(all.equal(getIntern(nlt, "assayIndex"), getIntern(lt, "assayIndex")))
#     testthat::expect_true(all.equal(assays(nlt, raw=TRUE), assays(lt, raw=TRUE)))
# })

# testthat::test_that("`reindex,LongTable-method` does not corrupt data relationships", {
#     nlt <- reindex(lt)
#     for (i in seq_along(assayNames(lt))) {
#         print(i)
#         assay1 <- assay(lt, i, withDimnames=TRUE)
#         setkeyv(assay1, idCols(lt))
#         assay2 <- assay(nlt, i, withDimnames=TRUE)
#         setkeyv(assay2, idCols(lt))
#         testthat::expect_true(all.equal(assay1, assay2))
#     }
#     assayL1 <- assays(lt)
#     assayL2 <- assays(nlt)
#     for (i in seq_along(assayL1)) {
#         print(i)
#         testthat::expect_true(all.equal(assayL1[[i]], assayL2[[i]]))
#     }
# })

# testthat::test_that("`CoreGx:::.subsetByIndex` is equivalent to subsetting the raw data", {
#     keepRows <- rowData(lt, key=TRUE)[drug1id %in% drug1id[1:5], ]
#     fullAssay <- lt$sensitivity
#     rawSubset <- fullAssay[drug1id %in% keepRows$drug1id, ]
#     aindex <- mutable(getIntern(lt, "assayIndex"))
#     subindex <- aindex[rowKey %in% keepRows$rowKey, ]
#     nlt <- CoreGx:::.subsetByIndex(lt, subindex)
#     testthat::expect_true(
#         !anyNA(assays(nlt, raw=TRUE)[["sensitivity"]]$sensitivity)
#     )
#     assayByIndex <- nlt$sensitivity
#     testthat::expect_true(all.equal(rawSubset, assayByIndex))
# })

# testthat::test_that("`subset,LongTable-method` works with row and column names", {
#     nlt <- subset(lt, rownames(lt)[1:5], colnames(lt)[1:5])
#     testthat::expect_equal(rownames(nlt), rownames(lt)[1:5])
#     testthat::expect_true(all.equal(rowData(nlt), rowData(lt)[1:5, ]))
#     testthat::expect_equal(colnames(nlt), colnames(lt)[1:5])
#     testthat::expect_true(all.equal(colData(nlt), colData(lt)[1:5, ]))
# })

# testthat::test_that("`subset,LongTable-method` works with call queries", {
#     nlt <- subset(lt,
#         drug1id %in% unique(drug1id)[1:5],
#         cellid %in% unique(cellid)[1:5]
#     )
#     testthat::expect_s4_class(nlt, "LongTable")
#     testthat::expect_equal(
#         rowData(nlt),
#         rowData(lt)[drug1id %in% unique(drug1id)[1:5]]
#     )
#     testthat::expect_equal(
#         colData(nlt),
#         colData(lt)[cellid %in% unique(cellid)[1:5]]
#     )
#     # check for NA values in the key column of the assay
#     testthat::expect_true(
#         !anyNA(assays(nlt, raw=TRUE)[["sensitivity"]]$sensitivity)
#     )
#     nlt2 <- lt[
#         .(drug1id %in% unique(drug1id)[1:5]),
#         .(cellid %in% unique(cellid)[1:5])
#     ]
#     testthat::expect_s4_class(nlt2, "LongTable")
#     testthat::expect_equal(
#         rowData(nlt2),
#         rowData(lt)[drug1id %in% unique(drug1id)[1:5]]
#     )
#     testthat::expect_equal(
#         colData(nlt2),
#         colData(lt)[cellid %in% unique(cellid)[1:5]]
#     )
#     # check for NA values in the key column of the assay
#     testthat::expect_true(
#         !anyNA(assays(nlt2, raw=TRUE)[["sensitivity"]]$sensitivity)
#     )
#     testthat::expect_equal(nlt, nlt2)
# })

# testthat::test_that("`subset,LongTable-method` works with regex queries", {
#     nlt <- subset(lt,
#         c("vemurafenib", "Vismodegib"),
#         c("UACC*", "SK-MEL-*")
#     )
#     testthat::expect_s4_class(nlt, "LongTable")
#     testthat::expect_equal(
#         rowData(nlt),
#         rowData(lt)[grepl("vemurafenib|Vismodegib", rownames(lt)), ]
#     )
#     testthat::expect_equal(
#         colData(nlt),
#         colData(lt)[grepl("UACC*|SK-MEL-*", colnames(lt)), ]
#     )
#     nlt2 <- lt[
#         c("vemurafenib", "Vismodegib"),
#         c("UACC*", "SK-MEL-*")
#     ]
#     testthat::expect_s4_class(nlt2, "LongTable")
#     testthat::expect_equal(
#         rowData(nlt2),
#         rowData(lt)[grepl("vemurafenib|Vismodegib", rownames(lt)), ]
#     )
#     testthat::expect_equal(
#         colData(nlt2),
#         colData(lt)[grepl("UACC*|SK-MEL-*", colnames(lt)), ]
#     )
#     testthat::expect_equal(nlt, nlt2)
# })