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
        unlockBinding("colIDs", getIntern(treatmentResponse))
        assign("colIDs",
            setNames(gsub("cellid", "sampleid", colIDs(treatmentResponse)),
                gsub("cellid", "sampleid", colIDs(treatmentResponse))),
            envir=getIntern(treatmentResponse)
        )
        lockBinding("colIDs", getIntern(treatmentResponse))
        colData_ <- colData(treatmentResponse)
        data.table::setnames(colData_, "cellid", "sampleid")
        colData(treatmentResponse) <- colData_
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

    ## TODO:: change any occurance of cellid to sample id in the old sensitivity
    ## slot list

    cSet <- .CoreSet(
        sample=sample_,
        treatment=treatment,
        sensitivity=treatmentResponse,
        molecularProfiles=mProf,
        annotation=object@annotation,
        curation=curation_,
        perturbation=object@perturbation,
        datasetType=object@datasetType
    )

    if (verify) isValid(cSet)

    return(cSet)
})