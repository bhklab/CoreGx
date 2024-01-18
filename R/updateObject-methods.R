#' Update the `CoreSet` class after changes in it struture or API
#'
#' @param object A `CoreSet` object to update the class structure for.
#' @param verify A `logical(1)` indicating is `validObject` should be called
#' after updating the object. Defaults to `TRUE`, only set `FALSE` for debugging.
#' @param verbose TRUE or FALSE, indicating whether information about the update
#' should be reported
#' @return `CoreSet` with update class structure.
#'
#' @md
#'
#' @importFrom MultiAssayExperiment MultiAssayExperiment
#' @importMethodsFrom BiocGenerics updateObject
#' @export
setMethod('updateObject', signature(object="CoreSet"),
        function(object, verify=FALSE, verbose = FALSE) {

    if (verbose) {
        message("updateObject object = 'CoreSet'")
    }
    
    if (!.hasSlot(object, "sample")) {
        cell <- object@cell
        sample_ <- cell
    } else {
        sample_ <- object@sample
    }
    colnames(sample_) <- gsub("cellid", "sampleid", colnames(sample_))

    if (!.hasSlot(object, "treatment")) {
        if (.hasSlot(object, "drug")) {
            treatment <- object@drug
        } else if (.hasSlot(object, "radiation")) {
            treatment <- object@radiation
        } else  {
            treatment <- data.frame()
        }
    } else {
        treatment <- object@treatment
    }
    colnames(treatment) <- gsub("drugid", "treatmentid", colnames(treatment))

    if (!.hasSlot(object, "treatmentResponse")) {
        treatmentResponse <- object@sensitivity
    } else {
        treatmentResponse <- object@treatmentResponse
    }

    if (is(treatmentResponse, "LongTable")) {
        treatmentResponse <- updateObject(treatmentResponse)
        mutableIntern <- mutable(getIntern(treatmentResponse))
    } else {
        colnames(treatmentResponse$info) <- gsub("cellid", "sampleid",
            colnames(treatmentResponse$info))
        colnames(treatmentResponse$info) <- gsub("drugid",
            "treatmentid", colnames(treatmentResponse$info))
    }

    mProf <- object@molecularProfiles
    for (i in seq_along(mProf)) {
        colnames(colData(mProf[[i]])) <- gsub("cellid", "sampleid",
            colnames(colData(mProf[[i]])))
        colnames(colData(mProf[[i]])) <- gsub("drugid", "treatmentid",
            colnames(colData(mProf[[i]])))
    }
    curation_ <- object@curation
    names(curation_) <- gsub("cell", "sample", names(curation_))
    names(curation_) <- gsub("drug", "treatment", names(curation_))
    colnames(curation_$sample) <- gsub("cellid", "sampleid",
        colnames(curation_$sample))
    if ("treatment" %in% names(curation_)) {
        colnames(curation_$treatment) <- gsub("drugid",
            "treatmentid", colnames(curation_$treatment))
    }

    cSet <- .CoreSet(
        sample=sample_,
        treatment=treatment,
        treatmentResponse=treatmentResponse,
        molecularProfiles=mProf,
        annotation=object@annotation,
        curation=curation_,
        perturbation=object@perturbation,
        datasetType=object@datasetType
    )

    if (verify) isValid(cSet)

    return(cSet)
})

#' Update the `LongTable` class after changes in it struture or API
#'
#' @param object A `LongTable` object to update the class structure for.
#' @param verify A `logical(1)` indicating is `validObject` should be called
#' after updating the object. Defaults to `TRUE`, only set `FALSE` for debugging.
#' @param verbose TRUE or FALSE, indicating whether information about the update
#' should be reported
#' @return `LongTable` with update class structure.
#'
#' @md
#'
#' @importMethodsFrom BiocGenerics updateObject
#' @export
setMethod("updateObject", signature(object="LongTable"),
        function(object, verify=FALSE, verbose = FALSE) {

     if (verbose) {
        message("updateObject object = 'CoreSet'")
    }
    if (is.environment(getIntern(object))) {
        rData <- rowData(object, key=TRUE)
        rIDs <- rowIDs(object)
        cData <- colData(object, key=TRUE)
        cIDs <- colIDs(object)
        id_cols <- c(rIDs, cIDs)
        assays_ <- assays(object, raw=TRUE)
        setkeyv(rData, "rowKey")
        assays_ <- lapply(assays_, merge.data.table,
            y=rData[, c("rowKey", rIDs), with=FALSE],
            by="rowKey"
        )
        setkeyv(cData, "colKey")
        assays_ <- lapply(assays_, merge.data.table,
            y=cData[, c("colKey", cIDs), with=FALSE],
            by="colKey"
        )
        rData[, rowKey := NULL]
        cData[, colKey := NULL]
        for (a_ in assays_) a_[, c("rowKey", "colKey") := NULL]
        mdata <- metadata(object)
        assayMap <- lapply(assays_, function(x, y) y, y=id_cols)
        oclass <- class(object)[1]
        object <- LongTable(
            rowData=rData, rowIDs=rIDs,
            colData=cData, colIDs=cIDs,
            assays=assays_, assayIDs=assayMap,
            metadata=mdata
        )
        object <- as(object, oclass)  # Coerce to inherting class if needed
    }
    if (verify) isValid(object)
    return(object)
})