library(testthat)
library(data.table)

data(nci_TRE_small)
data(merckLongTable)

lt <- merckLongTable
tre <- nci_TRE_small

# == LongTable constructor

testthat::test_that("`LongTable` is coercible to TRE", {
    tre <- as(lt, "TreatmentResponseExperiment")
    testthat::expect_s4_class(tre, "TreatmentResponseExperiment")
})

testthat::test_that("`LongTable` constructor method works with valid inputs", {
    ## Extract required parameters to create an TRE object
    parameters <- formalArgs(LongTable)
    parameters <- parameters[
        !(parameters %in% c("metadata", "keep.rownames"))
    ]
    row_data <- rowData(tre)
    row_ids <- rowIDs(tre)
    col_data <- colData(tre)
    col_ids <- colIDs(tre)
    assays_ <- assays(tre)
    assay_ids <- replicate(3, idCols(tre), simplify = FALSE)
    names(assay_ids) <- assayNames(tre)
    ## regex lookaheads to check for ALL missing parameters
    regex <- paste0(sprintf("(?=.*%s)", parameters), collapse = "")
    regex <- paste0("(?s)^", regex) ## handle line breaks in error messages
    ## Line 87: Report all missing parameters in error message
    testthat::expect_error({ ntre <- LongTable() },
        regexp = regex, perl = TRUE
    )
    ## Check for wrong input rowData class (those not coercible to data.frame)
    ## FIX-ME:: We might need extra check for rowData, colData, assays: even NULL is coercible to data.table
    #testthat::expect_error({
    #    ntre <- LongTable(rowData  = NULL,
    #                      rowIDs   = row_ids,
    #                      colData  = col_data,
    #                      colIDs   = col_ids,
    #                      assays   = assays_,
    #                      assayIDs = array_ids)
    #},
    #    regexp = ".*rowData must be coerceible to a data\\.frame"
    #)
    ## Question: should we handle the case where assays' IDs are mislabeled?
    ## Question: should we check for unequal length of names(assays) and names(assayIDs)? (refer to line 171)
    ## Line 172
    testthat::expect_error({
        ntre <- LongTable(rowData = row_data[, -row_ids[1:2], with = FALSE],
                          rowIDs = row_ids,
                          colData = col_data,
                          colIDs = col_ids,
                          assays = assays_,
                          assayIDs = array_ids)
    },
        regexp = paste0(".*Row IDs not in rowData: ",
                        row_ids[1:2],
                        collapse = ",")
    )
    ## Question: should we handle the case where assays' IDs are mislabeled?
    ## Question: should we check for unequal length of names(assays) and names(assayIDs)? (refer to line 171)
    ## Line 172
    testthat::expect_error({
        names(assay_ids)[1] <- "not sensitivity"
        ntre <- LongTable(rowData = row_data,
                          rowIDs = row_ids,
                          colData = col_data,
                          colIDs = col_ids,
                          assays = assays_,
                          assayIDs = assay_ids)
    },
        regexp = paste0(".*Mismatched names between ",
                        "assays and assayIDs for\\:\n\t",
                        paste0(names(assays_)[
                                    names(assays_) != names(assay_ids)
                               ],
                               collapse = ", "),
                        ".*", collapse = "")
    )
})

# == assayCols

testthat::test_that("`assayCols,LongTable-method` retrieves specified assay's column names",{
    testthat::expect_error({ assay_cols <- assayCols(tre, i = 1:2) },
        regexp = ".*The i parameter only accepts a single assay name or index.*"
    )
    testthat::expect_error({
        assay_cols <- assayCols(tre, i = (length(assayNames(tre)) + 1))
        },
        regexp = ".*The specified index is invalid.*"
    )
    testthat::expect_error({
        assay_cols <- assayCols(tre, i = paste0(assayNames(tre), collapse = ""))
        },
        regexp = ".*The specified index is invalid.*"
    )
})
