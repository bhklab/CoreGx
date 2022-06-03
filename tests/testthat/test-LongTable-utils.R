library(testthat)
library(CoreGx)
library(data.table)

data(nci_TRE_small)
tre <- nci_TRE_small

# == subset

testthat::test_that("`subset,LongTable-method` works with call queries", {
    ntre <- subset(tre,
        drug1id %in% unique(drug1id)[1:5],
        cellid %in% unique(cellid)[1:5]
    )
    testthat::expect_s4_class(ntre, "LongTable")
    ## These tests need to be updated to use expect_true with .table_is_subset 
    ## instead of expect_equal due the fact that the subset,LongTable-method
    ## will drop additional rowKey or colKey values than those in the initial 
    ## subset statement if there are no assay observations using those keys.
    ## This change fixed #148, but now makes it impossible to store metadata
    ## when there are no observations, which may not be ideal?
    ## Alternative would be to rework the assayIndex to be free of NA values
    testthat::expect_true(
        CoreGx:::.table_is_subset(
            rowData(ntre),
            rowData(tre)[drug1id %in% unique(drug1id)[1:5]]
        )
    )
    testthat::expect_true(
        CoreGx:::.table_is_subset(
            colData(ntre),
            colData(tre)[cellid %in% unique(cellid)[1:5]]
        )
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
    testthat::expect_true(
        CoreGx:::.table_is_subset(
            rowData(ntre2),
            rowData(tre)[drug1id %in% unique(drug1id)[1:5]]
        )
    )
    testthat::expect_true(
        CoreGx:::.table_is_subset(
            colData(ntre2),
            colData(tre)[cellid %in% unique(cellid)[1:5]]
        )
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

testthat::test_that("`subset,LongTable-method` doesn't produce non-existing assay observations from joining", {
    all_assays <- assays(tre, key = FALSE, withDimnames = TRUE)
    select_row_idx <- seq.int(1, dim(tre)[1], by = 2)
    select_col_idx <- seq.int(1, dim(tre)[2], by = 2)
    sub_tre <- subset(tre, i = select_row_idx, j = select_col_idx)
    for (a in seq_along(all_assays)) {
        assay_sub <- assay(sub_tre, a, key = FALSE, withDimnames = TRUE)
        obs_exists <- dim(
            assay_sub[!all_assays[[a]], on = names(assay_sub)]
            )[1] == 0 ## anti-join to check elements not a subset
        testthat::expect_true({ obs_exists })
    }
})

testthat::test_that("`subset,LongTable-method` doesn't miss assay observations for either selected row/columns", {
    all_assays <- assays(tre, key = FALSE, withDimnames = TRUE)
    select_row_idx <- sample.int(n = dim(tre)[1], size = 1, replace = FALSE)
    sub_tre <- subset(tre, i = select_row_idx)
    select_row <- rowData(tre)[select_row_idx, rowIDs(tre), with = FALSE]
    for (a in seq_along(all_assays)) {
        assay_sub1 <- assay(sub_tre, a, key = FALSE, withDimnames = TRUE)
        assay_sub2 <- all_assays[[a]][select_row, ]
        testthat::expect_equal(assay_sub1, assay_sub2)
    }
    select_col_idx <- sample.int(n = dim(tre)[2], size = 1, replace = FALSE)
    sub_tre <- subset(tre, j = select_col_idx)
    select_col <- colData(tre)[select_col_idx, colIDs(tre), with = FALSE]
    for (a in seq_along(assay_names)) {
        assay_sub1 <- assay(sub_tre, a, key = FALSE, withDimnames = TRUE)
        assay_sub2 <- all_assays[[a]][cellid == select_col, ]
        testthat::expect_equal(assay_sub1, assay_sub2)
    }
    sub_tre <- tre[select_row_idx, select_col_idx]
    select_both <- cbind(select_row, select_col)
    for (a in seq_along(assay_names)) {
        assay_sub1 <- assay(sub_tre, a, key = FALSE, withDimnames = TRUE)
        assay_sub2 <- all_assays[[a]][select_both, ]
        testthat::expect_equal(assay_sub1, assay_sub2)
    }
})

testthat::test_that("`subset,LongTable-method` subset indexing behaves the same as data.table", {
    ## Line 221
    sub_tre <- subset(tre, i = NULL) ## subset with row index by NULL
    testthat::expect_equal(dim(sub_tre), c(0, 0))
    sub_tre <- subset(tre, j = NULL) ## subset with column index by NULL
    testthat::expect_equal(dim(sub_tre), c(0, 0))
    sub_tre <- subset(tre, i = "") ## subset with row by empty rowname string
    testthat::expect_equal(sub_tre, tre)
    sub_tre <- subset(tre, j = "") ## subset with column by empty column name
    testthat::expect_equal(sub_tre, tre)
    sub_tre <- subset(tre, i = "", j = "") ## subset by empty row+column names
    testthat::expect_equal(sub_tre, tre)
    ## Get a subset with 2-Fluoro Ara-A of dose 6e-06 as second drug in combination therapies
    sub_tre <- subset(tre, i = "*:2-Fluoro Ara-A:*:6e-06")
    regex <- "(?=.*\\:2-Fluoro Ara-A)(?=.*6e-06\\:*)^" ## rowData regex
    testthat::expect_equal(
        rowData(tre)[grepl(regex, rownames(tre), perl = TRUE), ],
        rowData(sub_tre)
    ) ## much nicer to query at the TRE level
    ## Subset containing ovarian cancer cell line
    sub_tre <- subset(tre, j = ".*OVCAR.*")
    testthat::expect_equal(
        colData(tre)[grepl(".*OVCAR.*", colnames(tre), perl = TRUE), ],
        colData(sub_tre)
    )
    ## Subset by negative index: TRE behaves the same as data.table
    i <- sample.int(n = dim(tre)[1], size = 1, replace = FALSE)
    sub_tre_1 <- tre[-i, ] ## Drop the i-th row
    sub_tre_2 <- tre[i, ] ## Extract the i-th row
    testthat::expect_equal(
        rowData(tre)[!rowData(sub_tre_1), on = names(rowData(tre))],
        rowData(sub_tre_2) # rowData(tre)\rowData(sub_tre_1)=rowData(sub_tre_2)
    )
    j <- sample.int(n = dim(tre)[2], size = 1, replace = FALSE)
    sub_tre_3 <- tre[, -j] ## Drop the j-th column
    sub_tre_4 <- tre[, j] ## Extract the j-th column
    testthat::expect_equal(
        colData(tre)[!colData(sub_tre_3), on = names(colData(tre))],
        colData(sub_tre_4) # colData(tre)\colData(sub_tre_3)=colData(sub_tre_4)
    )
    sub_tre_5 <- tre[-i, -j] ## Drop data containing i-th row OR j-th column
    sub_tre_6 <- tre[i, j] ## Extract i-th row AND j-th column
    all_assays <- assays(tre, key = FALSE, withDimnames = TRUE)
    for (a in seq_along(all_assays)) {
        testthat::expect_equal(
            all_assays[[a]][
                !assay(sub_tre_1, a, key = FALSE, withDimnames = TRUE),
            ],
            assay(sub_tre_2, i = a, key = FALSE, withDimnames = TRUE)
        )
        testthat::expect_equal(
            all_assays[[a]][
                !assay(sub_tre_3, a, key = FALSE, withDimnames = TRUE),
            ],
            assay(sub_tre_4, i = a, key = FALSE, withDimnames = TRUE)
        )
        ## tre[i, ] UNION tre[, j] + (tre[i, ] INTERSECT tre[, j])
        union_assay <- rbind(
            assay(sub_tre_2, i = a, key = FALSE, withDimnames = TRUE),
            assay(sub_tre_4, i = a, key = FALSE, withDimnames = TRUE)
        ) ## contains double count, not a union yet
        double_count_idx <- which(duplicated(union_assay))
        # Show that the double counted element is the intersect
        testthat::expect_equal(
            union_assay[double_count_idx, ],
            ## tre[i, ] INTERSECT tre[, j]
            assay(sub_tre_6, i = a, key = FALSE, withDimnames = TRUE),
            ignore_attr = TRUE
        )
        ## Show that these two produce equivalent union sets
        union_assay <- setorderv(union_assay[-double_count_idx, ],
                                 cols = idCols(tre))
        testthat::expect_equal(
            union_assay,
            ## This indirect approach for tre[i, ] UNION tre[, j] is faster
            all_assays[[a]][
                !assay(sub_tre_5, a, key = FALSE, withDimnames = TRUE),
                on = idCols(tre)
            ], ## reordering done by internal reindexing
            ignore_attr = TRUE
        )
    }
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
        setkeyv(assay2, idCols(ntre))
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
    select_row <- seq.int(1, dim(tre)[1], by = 2)
    stre <- tre[select_row, ] ## subset data
    stre <- reindex(stre)
    ## check if rowData and colData keys have gaps
    row_keys <- rowData(stre, key = TRUE)$rowKey
    col_keys <- colData(stre, key = TRUE)$colKey
    has_no_gaps_in_row <- rle(diff(row_keys))$value == 1
    has_no_gaps_in_col <- rle(diff(col_keys))$value == 1
    testthat::expect_true(has_no_gaps_in_row)
    testthat::expect_true(has_no_gaps_in_col)
    ## check if assays' keys have gaps
    for (i in seq_along(assayNames(stre))) {
        assay_name <- assayNames(stre)[i]
        assay_keys <- assay(stre, i, key = FALSE, withDimnames = FALSE)[[assay_name]]
        has_no_gaps_in_assay <- rle(diff(assay_keys))$value == 1
        if (length(has_no_gaps_in_assay) > 1) print(i)
        testthat::expect_true(has_no_gaps_in_assay)
    } # Leave summary assay out for now
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