#' Update the `CoreSet` class after changes in it struture or API
#'
#' @param object A `CoreSet` object to update the class structure for.
#'
#' @return `CoreSet` with update class structure.
#'
#' @md
#'
#' @importFrom MultiAssayExperiment MultiAssayExperiment
#' @importMethodsFrom BiocGenerics updateObject
#' @export
setMethod('updateObject', signature(object="CoreSet"),
        function(object, verify=FALSE) {
    if (!.hasSlot(object, "sample")) {
        cell <- object@cell
        sample_ <- cell
    } else {
        sample_ <- object@sample
    }
    colnames(sample_) <- gsub("cellid", "sampleid", colnames(sample_))

    if (!.hasSlot(object, "treatment")) {
        treatment <- data.frame()
    } else {
        treatment <- object@treatment
    }

    if (!.hasSlot(object, "treatmentResponse")) {
        treatmentResponse <- object@sensitivity
    } else {
        treatmentResponse <- object@treatmentResponse
    }

    if (is(treatmentResponse, "LongTable")) {
        if (!("sampleid" %in% colIDs(treatmentResponse))) {
            unlockBinding("colIDs", getIntern(treatmentResponse))
            assign("colIDs",
                setNames(gsub("cellid", "sampleid", colIDs(treatmentResponse)),
                    gsub("cellid", "sampleid", colIDs(treatmentResponse))),
                envir=getIntern(treatmentResponse)
            )
            lockBinding("colIDs", getIntern(treatmentResponse))
        }
        if (!("sampleid" %in% colnames(colData(treatmentResponse)))) {
                    colData_ <- colData(treatmentResponse)
            data.table::setnames(colData_, "cellid", "sampleid")
            ## FIXME:: this results in incorrect order!
            colData(treatmentResponse) <- colData_
        }
    } else {
        if (!("sampleid" %in% colnames(treatmentResponse$info))) {
            treatmentResponse$info$sampleid <- treatmentResponse$info$cellid
        }
    }

    mProf <- object@molecularProfiles
    for (i in seq_along(mProf)) {
        colnames(colData(mProf[[i]])) <- gsub("cellid", "sampleid",
            colnames(colData(mProf[[i]])))
    }
    if (!is(mProf, "MultiAssayExperiment")) mProf <- MultiAssayExperiment(mProf)

    curation_ <- object@curation
    names(curation_) <- gsub("cell", "sample", names(curation_))
    colnames(curation_$sample) <- gsub("cellid", "sampleid", names(curation_))
    if (!("treatment" %in% names(curation_))) {
        curation_$treatment <- data.frame()
    }

    ## TODO:: change any occurance of cellid to sample id in the old sensitivity
    ## slot list

    cSet <- CoreSet2(
        name=name(object),
        sample=sample_,
        treatment=treatment,
        treatmentResponse=treatmentResponse,
        molecularProfiles=mProf,
        curation=curation_,
        perturbation=object@perturbation,
        datasetType=object@datasetType
    )

    if (verify) isValid(cSet)

    return(cSet)
})