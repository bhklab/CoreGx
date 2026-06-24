library(testthat)
library(CoreGx)
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

testthat::test_that("`LongTable` constructor builds sparse assay index", {
    row_data <- data.table(
        treatmentid = paste0("D", seq_len(100)),
        row_meta = seq_len(100)
    )
    col_data <- data.table(
        sampleid = paste0("S", seq_len(100)),
        col_meta = seq_len(100)
    )
    sensitivity <- data.table(
        treatmentid = c("D1", "D20", "D100"),
        sampleid = c("S2", "S2", "S99"),
        viability = c(0.9, 0.5, 0.1)
    )

    lt <- LongTable(
        rowData = row_data,
        rowIDs = "treatmentid",
        colData = col_data,
        colIDs = "sampleid",
        assays = list(sensitivity = sensitivity),
        assayIDs = list(sensitivity = c("treatmentid", "sampleid"))
    )

    testthat::expect_equal(nrow(assayIndex(lt)), nrow(sensitivity))
    testthat::expect_equal(
        nrow(assay(lt, "sensitivity", withDimnames = TRUE)),
        nrow(sensitivity)
    )
})

testthat::test_that("`LongTable` constructor preserves assay-specific keys", {
    row_data <- data.table(
        experiment_id = "E1",
        treatmentid = "D1"
    )
    col_data <- data.table(sampleid = "S1")
    raw <- data.table(
        experiment_id = c("E1", "E1"),
        sampleid = c("S1", "S1"),
        dose_index = c(1L, 2L),
        dose = c(-7, -6),
        viability = c(95, 70)
    )

    lt <- LongTable(
        rowData = row_data,
        rowIDs = "experiment_id",
        colData = col_data,
        colIDs = "sampleid",
        assays = list(raw = raw),
        assayIDs = list(raw = c("experiment_id", "sampleid", "dose_index"))
    )

    raw_assay <- assay(lt, "raw", withDimnames = TRUE, summarize = FALSE)
    testthat::expect_equal(
        assayKeys(lt, "raw"),
        c("experiment_id", "sampleid", "dose_index")
    )
    testthat::expect_equal(nrow(raw_assay), 2L)
    testthat::expect_equal(raw_assay$dose_index, c(1L, 2L))
    testthat::expect_true("dose_index" %in% colnames(assays(lt, raw = TRUE)$raw))
    testthat::expect_false("experiment_id" %in% colnames(assays(lt, raw = TRUE)$raw))
    testthat::expect_false("sampleid" %in% colnames(assays(lt, raw = TRUE)$raw))
})

testthat::test_that("`LongTable` constructor expands summary assays by observed keys", {
    row_data <- data.table(
        treatmentid = c("D1", "D1"),
        dose = c(-7, -6)
    )
    col_data <- data.table(sampleid = "S1")
    profiles <- data.table(
        treatmentid = "D1",
        sampleid = "S1",
        ic50 = -6.5
    )

    lt <- LongTable(
        rowData = row_data,
        rowIDs = c("treatmentid", "dose"),
        colData = col_data,
        colIDs = "sampleid",
        assays = list(profiles = profiles),
        assayIDs = list(profiles = c("treatmentid", "sampleid"))
    )

    testthat::expect_equal(
        nrow(assay(lt, "profiles", summarize = FALSE)),
        nrow(row_data)
    )
    testthat::expect_equal(nrow(assay(lt, "profiles")), nrow(profiles))
})

testthat::test_that("`.rebuildRaw` supports paired numbered wide raw assays", {
    row_data <- data.table(
        experiment_id = c("E1", "E2"),
        treatmentid = c("D1", "D2")
    )
    col_data <- data.table(sampleid = c("S1", "S2"))
    sensitivity <- data.table(
        experiment_id = c("E1", "E2"),
        sampleid = c("S1", "S2"),
        dose_1 = c(1e-8, 2e-8),
        dose_2 = c(1e-7, 2e-7),
        dose_10 = c(1e-6, 2e-6),
        viability_1 = c(95, 90),
        viability_2 = c(75, 70),
        viability_10 = c(20, 10)
    )

    tre <- TreatmentResponseExperiment(
        rowData = row_data,
        rowIDs = "experiment_id",
        colData = col_data,
        colIDs = "sampleid",
        assays = list(sensitivity = sensitivity),
        assayIDs = list(sensitivity = c("experiment_id", "sampleid"))
    )

    raw <- CoreGx:::.rebuildRaw(tre)

    testthat::expect_equal(dim(raw), c(2L, 3L, 2L))
    testthat::expect_equal(dimnames(raw)[[1]], c("E1_S1", "E2_S2"))
    testthat::expect_equal(dimnames(raw)[[2]], c("dose1", "dose2", "dose10"))
    testthat::expect_equal(
        unname(raw[, , "Dose"]),
        matrix(c(1e-8, 2e-8, 1e-7, 2e-7, 1e-6, 2e-6), nrow = 2)
    )
    testthat::expect_equal(
        unname(raw[, , "Viability"]),
        matrix(c(95, 90, 75, 70, 20, 10), nrow = 2)
    )
})

# == assayCols

testthat::test_that("`assayCols,LongTable-method` retrieves specified assay's column names",{
    testthat::expect_error({ assayCols(tre, i = 1:2) })
    testthat::expect_error({ assayCols(tre, i = (length(assayNames(tre))) + 1) })
    testthat::expect_error({ assayCols(tre, i = paste0(assayNames(tre), collapse = "")) })
})
