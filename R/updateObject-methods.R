#'
#' 
#' 
#' 
#' 
#' 
#' @importMethodsFrom BiocGenerics updateObject
setMethod('updateObject', signature(object="CoreSet"),
        function(object, verify=FALSE) {
    if (!.hasSlot(object, "sample")) {
        sample <- object@cell
    } else {
        sample <- object@sample
    }

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

    cSet <- .CoreSet(
        sample=sample,
        treatment=treatment,
        treatmentResponse=treatmentResponse,
        molecularProfiles=object@molecularProfiles,
        annotation=object@annotation,
        curation=object@curation,
        perturbation=object@perturbation,
        datasetType=object@datasetType
    )

    if (verify) isValid(cSet)

    return(cSet)
})